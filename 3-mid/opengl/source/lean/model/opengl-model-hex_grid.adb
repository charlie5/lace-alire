with
     openGL.Geometry.colored,
     openGL.Primitive.indexed,

     float_Math.Geometry.d2.Hexagon,

     ada.Containers.hashed_Maps,
     ada.unchecked_Deallocation;

with ada.Text_IO; use ada.Text_IO;


package body openGL.Model.hex_grid
is
   --------
   -- Forge
   --

   function new_Grid (heights_Asset : in asset_Name;
                      Heights       : in height_Map_view) return View
   is
      the_Model : constant View := new Item' (Model.item with
                                              heights_Asset => heights_Asset,
                                              Heights       => Heights);
   begin
      the_Model.set_Bounds;
      return the_Model;
   end new_Grid;



   overriding
   procedure destroy (Self : in out Item)
   is
      procedure deallocate is new ada.unchecked_Deallocation (height_Map,
                                                              height_Map_view);
   begin
      destroy (Model.Item (Self));
      deallocate (Self.Heights);
   end destroy;


   -------------
   -- Attributes
   --

   package hexagon_Geometry  renames Geometry_2d.Hexagon;


   -- site_Map_of_vertex_Id
   --

   function Hash (From : in Geometry_2d.Site) return ada.Containers.Hash_type
   is
      use ada.Containers;

      type Fix is delta 0.00_1 range 0.0 .. 1000.0;

      cell_Size  : constant Fix := 0.5;
      grid_Width : constant     := 10;
   begin
      return   Hash_type (Fix (From (1)) / cell_Size)
             + Hash_type (Fix (From (2)) / cell_Size) * grid_Width;
   end Hash;


   function Equivalent (S1, S2 : Geometry_2d.Site) return Boolean
   is
      Tolerance : constant := 0.1;
   begin
      return     abs (S2 (1) - S1 (1)) < Tolerance
             and abs (S2 (2) - S1 (2)) < Tolerance;
   end Equivalent;



   type Coordinates_array is array (Index_t range <>) of hexagon_Geometry.Coordinates;

   type hex_Vertex is
      record
         shared_Hexes : Coordinates_array (1 .. 3);
         shared_Count : Index_t := 0;

         Site         : Geometry_3d.Site;
      end record;

   type hex_Vertices is array (Index_t range <>) of hex_Vertex;



   package site_Maps_of_vertex_Id is new ada.Containers.hashed_Maps (Key_type        => Geometry_2d.Site,
                                                                     Element_type    => Index_t,
                                                                     Hash            => Hash,
                                                                     equivalent_Keys => Equivalent,
                                                                     "="             => "=");


   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use Geometry,
          Geometry.colored,
          Geometry_2d;

      site_Map_of_vertex_Id : site_Maps_of_vertex_Id.Map;
        next_free_vertex_Id : Index_t := 0;


      function fetch_Id (S : in Geometry_2d.Site) return Index_t
      is
         use site_Maps_of_vertex_Id;
         C : constant Cursor := site_Map_of_vertex_Id.Find (S);
      begin
         --  put_Line (S'Image);
         if has_Element (C)
         then
            --  put_Line (Element (C)'Image);
            return Element (C);
         else
            next_free_vertex_Id := @ + 1;
            site_Map_of_vertex_Id.insert (S, next_free_vertex_Id);
            --  put_Line (next_free_vertex_Id'Image);
            return next_free_vertex_Id;
         end if;
      end fetch_Id;


      Heights   : height_Map_view renames Self.Heights;

      row_Count : constant Index_t := Heights'Length (1);
      col_Count : constant Index_t := Heights'Length (2);

      the_Grid  : constant hexagon_Geometry.Grid := Hexagon.to_Grid (Rows         => Positive (row_Count),
                                                                     Cols         => Positive (col_Count),
                                                                     circumRadius => 1.0);
      zig_zag_Count : constant Index_t := col_Count + 1;

      first_zig_zag_vertex_Count : constant Index_t := row_Count * 2 + 1;
        mid_zig_zag_vertex_Count : constant Index_t := row_Count * 2 + 2;
       last_zig_zag_vertex_Count : constant Index_t := row_Count * 2 + 1;

      zig_zags_vertex_Count      : constant Index_t :=   first_zig_zag_vertex_Count
                                                       + (mid_zig_zag_vertex_Count) * (zig_zag_Count - 2)
                                                       + last_zig_zag_vertex_Count;
      zig_zag_joiner_vertex_Count : constant Index_t := col_Count * 2;

      vertex_Count  : constant Index_t :=         zig_zags_vertex_Count
                                          + zig_zag_joiner_vertex_Count;

      hex_Vertices  : hex_Grid.hex_Vertices (1 .. zig_zags_vertex_Count);

      --  indices_Count : constant long_Index_t :=   (2 * (long_Index_t (Heights'Length (2)) + 1)) * (long_Index_t (row_Count) - 1)
      --                                           +  2 * (long_Index_t (Heights'Length (2)));
      indices_Count : constant long_Index_t := long_Index_t (vertex_Count);

      the_Vertices  : aliased  Geometry.colored.Vertex_array := [1 ..  vertex_Count => <>];
      the_Indices   : aliased  Indices                       := [1 .. indices_Count => <>];

      the_Geometry  : constant Geometry.colored.view := Geometry.colored.new_Geometry;

   begin

      find_shared_Hexes_per_Vertex:
      begin
         for Row in 1 .. row_Count
         loop

            for Col in 1 .. col_Count
            loop

               for Which in hexagon_Geometry.vertex_Id
               loop
                  declare
                     use hexagon_Geometry;
                     Site       : constant Geometry_2d.Site := vertex_Site (the_Grid,
                                                                            hex_Id => [Positive (Row),
                                                                                       Positive (Col)],
                                                                            Which  => Which);

                     vertex_Id  : constant Index_t    :=      fetch_Id (S => Site);
                     the_Vertex :          hex_Vertex renames hex_Vertices (vertex_Id);
                     C          : constant Index_t    :=      the_Vertex.shared_Count + 1;
                  begin
                     the_Vertex.shared_Count     := C;
                     the_Vertex.shared_Hexes (C) := [Positive (Row),
                                                     Positive (Col)];
                     the_Vertex.Site := [Site (1),
                                         0.0,
                                         Site (2)];
                  end;
               end loop;

            end loop;

         end loop;
      end find_shared_Hexes_per_Vertex;


      set_Height_for_each_Vertex:
      begin
         for Row in 1 .. row_Count
         loop

            for Col in 1 .. col_Count
            loop

               for Which in hexagon_Geometry.vertex_Id
               loop
                  declare
                     use hexagon_Geometry;
                     Site       : constant Geometry_2d.Site := vertex_Site (the_Grid,
                                                                            hex_Id => [Positive (Row),
                                                                                       Positive (Col)],
                                                                            Which  => Which);
                     Height     :          Real       :=      0.0;
                     vertex_Id  : constant Index_t    :=      fetch_Id (S => Site);
                     the_Vertex :          hex_Vertex renames hex_Vertices (vertex_Id);
                  begin
                     for Each in 1 .. the_Vertex.shared_Count
                     loop
                        Height := Height + Heights (Row, Col);
                     end loop;

                     Height          := Height / Real (the_Vertex.shared_Count);
                     the_Vertex.Site := [Site (1),
                                         Height,
                                         Site (2)];
                  end;
               end loop;

            end loop;

         end loop;
      end set_Height_for_each_Vertex;


      set_GL_Vertices:
      declare
         vertex_Id : Index_t := 0;
      begin
         --- Add hex vertices.
         --
         for i in hex_Vertices'Range
         loop
            vertex_Id := vertex_Id + 1;
            the_Vertices (vertex_Id).Site  := hex_Vertices (vertex_Id).Site;
            the_Vertices (vertex_Id).Color := (Primary => [255, 255, 255],
                                               Alpha   => 255);
         end loop;

         --- Add joiner vertices.
         --
         for i in 1 .. col_Count
         loop
            declare
               use hexagon_Geometry;
               Site : Geometry_2d.Site := vertex_Site (the_Grid,
                                                       hex_Id => [Row => Positive (row_Count),
                                                                  Col => Positive (i)],
                                                       Which  => 3);
            begin
               vertex_Id                := vertex_Id + 1;
               the_Vertices (vertex_Id) := (Site => [Site (1), 0.0, Site (2)],
                                            Color => (Primary => [0, 0, 0],
                                                      Alpha   => 0));

               Site                     := vertex_Site (the_Grid,
                                                        hex_Id => [Row => 1,
                                                                   Col => Positive (i)],
                                                        Which  => 6);
               vertex_Id                := vertex_Id + 1;
               the_Vertices (vertex_Id) := (Site => [Site (1), 0.0, Site (2)],
                                            Color => (Primary => [0, 0, 0],
                                                      Alpha   => 0));
            end;
         end loop;
      end set_GL_Vertices;


      set_GL_Indices:
      declare
         Cursor            : long_Index_t := 0;
         joiners_vertex_Id :      Index_t := zig_zags_vertex_Count;

         procedure add_zig_zag_Vertex (Row, Col   : in Positive;
                                       hex_Vertex : in Hexagon.vertex_Id)
         is
            use hexagon_Geometry;

            Site : constant Geometry_2d.Site := vertex_Site (the_Grid,
                                                             hex_Id => [Row, Col],
                                                             Which  => hex_Vertex);
         begin
            Cursor               := Cursor + 1;
            the_Indices (Cursor) := fetch_Id (S => Site);
         end add_zig_zag_Vertex;

         procedure add_joiner_vertex_Pair
         is
         begin
            Cursor               := Cursor + 1;
            joiners_vertex_Id    := joiners_vertex_Id + 1;
            the_Indices (Cursor) := joiners_vertex_Id;

            Cursor               := Cursor + 1;
            joiners_vertex_Id    := joiners_vertex_Id + 1;
            the_Indices (Cursor) := joiners_vertex_Id;
         end;


         --  the_height_Range : constant Vector_2 := height_Extent (Heights.all);
         --  Middle           : constant Real     := (the_height_Range (1) + the_height_Range (2))  /  2.0;

      begin
         --- Fist zigzag
         --
         add_zig_zag_Vertex (Row => 1, Col => 1, hex_Vertex => 5);

         for Row in 1 .. Positive (row_Count)
         loop
            add_zig_zag_Vertex (Row, Col => 1, hex_Vertex => 4);
            add_zig_zag_Vertex (Row, Col => 1, hex_Vertex => 3);
         end loop;

         add_joiner_vertex_Pair;


         --- Middles zigzags
         --

         for zz in 2 .. Positive (zig_zag_Count) - 1
         loop
            declare
               odd_Zigzag : constant Boolean := zz mod 2 = 1;
            begin

               if odd_Zigzag
               then
                  add_zig_zag_Vertex (Row => 1, Col => Positive (zz + 0), hex_Vertex => 5);

               else -- Even zigzag.
                  add_zig_zag_Vertex (Row => 1, Col => Positive (zz - 1), hex_Vertex => 6);
               end if;

               for Row in 1 .. Positive (row_Count)
               loop
                  if odd_Zigzag
                  then
                     add_zig_zag_Vertex (Row, Col => zz, hex_Vertex => 4);
                     add_zig_zag_Vertex (Row, Col => zz, hex_Vertex => 3);

                     if Row = Positive (row_Count)     -- Last row.
                     then
                        add_zig_zag_Vertex (Row, Col => zz - 1, hex_Vertex => 2);
                     end if;

                  else -- Even zigzag.
                     add_zig_zag_Vertex (Row, Col => zz, hex_Vertex => 5);
                     add_zig_zag_Vertex (Row, Col => zz, hex_Vertex => 4);

                     if Row = Positive (row_Count)     -- Last row.
                     then
                        add_zig_zag_Vertex (Row, Col => zz, hex_Vertex => 3);
                     end if;
                  end if;
               end loop;
            end;

            add_joiner_vertex_Pair;
         end loop;


         --- Last zigzag
         --
         add_zig_zag_Vertex (Row => 1, Col => Positive (col_Count), hex_Vertex => 6);

         for Row in 1 .. Positive (row_Count)
         loop
            add_zig_zag_Vertex (Row, Positive (col_Count), hex_Vertex => 1);
            add_zig_zag_Vertex (Row, Positive (col_Count), hex_Vertex => 2);
         end loop;

         --  add_joiner_vertex_Pair;
      end set_GL_Indices;


      the_Geometry.is_Transparent (False);
      the_Geometry.Vertices_are   (the_Vertices);

      declare
         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (Primitive.line_Strip,
                                               the_Indices);
      begin
         the_Geometry.add (Primitive.view (the_Primitive));
      end;

      return [1 => Geometry.view (the_Geometry)];
   end to_GL_Geometries;




















   --  overriding
   --  function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
   --                                                   Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   --  is
   --     pragma unreferenced (Textures, Fonts);
   --
   --     use Geometry,
   --         Geometry.colored,
   --         Geometry_2d;
   --
   --     Heights       :          height_Map_view renames Self.Heights;
   --
   --     row_Count     : constant Index_t := Heights'Length (1);
   --     col_Count     : constant Index_t := Heights'Length (2);
   --
   --     the_Grid      : constant hexagon_Geometry.Grid := Hexagon.to_Grid (Rows         => Positive (row_Count),
   --                                                                        Cols         => Positive (col_Count),
   --                                                                        circumRadius => 1.0);
   --     zig_zag_Count              : constant Index_t := col_Count + 1;
   --
   --     first_zig_zag_vertex_Count : constant Index_t := row_Count * 2 + 1;
   --     mid_zig_zag_vertex_Count   : constant Index_t := row_Count * 2 + 2;
   --     last_zig_zag_vertex_Count  : constant Index_t := row_Count * 2 + 1;
   --
   --     --  vertex_Count  : constant Index_t :=   first_zig_zag_vertex_Count
   --     --                                      +  (mid_zig_zag_vertex_Count) * (zig_zag_Count - 2)
   --     --                                      +  last_zig_zag_vertex_Count;
   --
   --     vertex_Count  : constant Index_t :=   first_zig_zag_vertex_Count
   --                                         --  +  (mid_zig_zag_vertex_Count) * (zig_zag_Count - 2)
   --                                         +  last_zig_zag_vertex_Count
   --                                         + 1;
   --
   --
   --     --  vertex_Count  : constant Index_t      := Heights'Length (1) * Heights'Length (2);
   --
   --     --  indices_Count : constant long_Index_t :=   (2 * (long_Index_t (Heights'Length (2)) + 1)) * (long_Index_t (row_Count) - 1)
   --     --                                           +  2 * (long_Index_t (Heights'Length (2)));
   --     indices_Count : constant long_Index_t := long_Index_t (vertex_Count);
   --
   --     the_Sites     : aliased  Sites         := [1 .. vertex_Count => <>];
   --     the_Bounds    :          openGL.Bounds := null_Bounds;
   --
   --     the_Vertices  : aliased  Geometry.colored.Vertex_array := [1 ..  vertex_Count => <>];
   --     the_Indices   : aliased  Indices                       := [1 .. indices_Count => <>];
   --
   --     the_Geometry  : constant Geometry.colored.view := Geometry.colored.new_Geometry;
   --
   --  begin
   --     set_Sites:
   --     declare
   --        vert_Id  : Index_t  := 0;
   --
   --        the_Site : Geometry_2d.Site;
   --        Offset   : Geometry_2d.Site;
   --
   --        Hex      : constant Geometry_2d.Hexagon.item := (circumRadius => 1.0);
   --
   --        procedure add_zig_zag (Row, Col   : in Positive;
   --                               hex_Vertex : in Hexagon.vertex_Id)
   --        is
   --        begin
   --           vert_Id := vert_Id + 1;
   --           put_Line (vert_Id'Image);
   --
   --           the_Site            := hexagon_Geometry.Site (the_Grid, Row, Col);
   --           Offset              := hexagon_Geometry.Site (Hex, Id => hex_Vertex);
   --
   --           the_Sites (vert_Id) :=   [the_Site (1), 0.0, the_Site (2)]
   --                                  + [  Offset (1), 0.0,   Offset (2)];
   --
   --           the_Vertices (vert_Id).Site  := the_Sites (vert_Id);
   --        end add_zig_zag;
   --
   --
   --        --  the_height_Range : constant Vector_2 := height_Extent (Heights.all);
   --        --  Middle           : constant Real     := (the_height_Range (1) + the_height_Range (2))  /  2.0;
   --
   --     begin
   --        for Col in 1 .. 1 -- col_Count
   --        loop
   --           for Row in 1 .. row_Count
   --           loop
   --              add_zig_zag (Positive (Row), Col, hex_Vertex => 5);
   --              add_zig_zag (Positive (Row), Col, hex_Vertex => 4);
   --
   --              if Row = row_Count     -- The last row.
   --              then
   --                 add_zig_zag (Positive (Row), Col, hex_Vertex => 3);
   --              end if;
   --
   --
   --              the_Bounds.Box.Lower (1) := Real'Min (the_Bounds.Box.Lower (1),  the_Sites (vert_Id) (1));
   --              the_Bounds.Box.Lower (2) := 0.0; -- Real'Min (the_Bounds.Box.Lower (2),  the_Sites (vert_Id) (2));
   --              the_Bounds.Box.Lower (3) := Real'Min (the_Bounds.Box.Lower (3),  the_Sites (vert_Id) (3));
   --
   --              the_Bounds.Box.Upper (1) := Real'Max (the_Bounds.Box.Upper (1),  the_Sites (vert_Id) (1));
   --              the_Bounds.Box.Upper (2) := 0.0; -- Real'Max (the_Bounds.Box.Upper (2),  the_Sites (vert_Id) (2));
   --              the_Bounds.Box.Upper (3) := Real'Max (the_Bounds.Box.Upper (3),  the_Sites (vert_Id) (3));
   --
   --              the_Bounds.Ball := Real'Max (the_Bounds.Ball,
   --                                           abs (the_Sites (vert_Id)));
   --           end loop;
   --
   --           for Row in 1 .. row_Count
   --           loop
   --              add_zig_zag (Positive (Row), Col, hex_Vertex => 6);
   --              add_zig_zag (Positive (Row), Col, hex_Vertex => 1);
   --
   --              if Row = row_Count     -- The last row.
   --              then
   --                 add_zig_zag (Positive (Row), Col, hex_Vertex => 2);
   --              end if;
   --
   --
   --              the_Bounds.Box.Lower (1) := Real'Min (the_Bounds.Box.Lower (1),  the_Sites (vert_Id) (1));
   --              the_Bounds.Box.Lower (2) := 0.0; -- Real'Min (the_Bounds.Box.Lower (2),  the_Sites (vert_Id) (2));
   --              the_Bounds.Box.Lower (3) := Real'Min (the_Bounds.Box.Lower (3),  the_Sites (vert_Id) (3));
   --
   --              the_Bounds.Box.Upper (1) := Real'Max (the_Bounds.Box.Upper (1),  the_Sites (vert_Id) (1));
   --              the_Bounds.Box.Upper (2) := 0.0; -- Real'Max (the_Bounds.Box.Upper (2),  the_Sites (vert_Id) (2));
   --              the_Bounds.Box.Upper (3) := Real'Max (the_Bounds.Box.Upper (3),  the_Sites (vert_Id) (3));
   --
   --              the_Bounds.Ball := Real'Max (the_Bounds.Ball,
   --                                           abs (the_Sites (vert_Id)));
   --           end loop;
   --        end loop;
   --
   --        the_Bounds.Ball := the_Bounds.Ball * 1.1;     -- TODO: Why the '* 1.1' ?
   --     end set_Sites;
   --
   --
   --     set_Indices:
   --     declare
   --        Cursor : long_Index_t := 0;
   --        Start,
   --        Upper,
   --        Lower  : Index_t;
   --     begin
   --        Start := 1;
   --
   --        for Col in 1 .. Index_t'(1) -- col_Count
   --        loop
   --           for Row in 1 .. row_Count
   --           loop
   --              Upper := Start;
   --              Lower := Start + 1;
   --
   --              Cursor := Cursor + 1;   the_Indices (Cursor) := Upper;
   --              Cursor := Cursor + 1;   the_Indices (Cursor) := Lower;
   --
   --              if Row = row_Count     -- Last row.
   --              then
   --                 Cursor := Cursor + 1;   the_Indices (Cursor) := Lower + 1;
   --              end if;
   --
   --              Start := Start + 2;
   --           end loop;
   --
   --           for Row in 1 .. row_Count
   --           loop
   --              Upper := Start;
   --              Lower := Start + 1;
   --
   --              Cursor := Cursor + 1;   the_Indices (Cursor) := Upper;
   --              Cursor := Cursor + 1;   the_Indices (Cursor) := Lower;
   --
   --              if Row = row_Count     -- Last row.
   --              then
   --                 Cursor := Cursor + 1;   the_Indices (Cursor) := Lower + 1;
   --              end if;
   --
   --              Start := Start + 2;
   --           end loop;
   --
   --           the_Vertices (vertex_Count).Site := [0.0, 0.0, 0.0];
   --           the_Indices (Cursor + 1) := 14;
   --
   --           --  if Row /= row_Count   -- Not the last row.
   --           --  then
   --           --     -- Add 1st redundant triangle to allow for next strip.
   --           --     Cursor := Cursor + 1;   the_Indices (Cursor) := Lower;
   --           --
   --           --     -- Advance Start index.
   --           --     Start  := Start + col_Count + 1;
   --           --
   --           --     -- Add 2nd redundant triangle to allow for next strip.
   --           --     Cursor := Cursor + 1;   the_Indices (Cursor) := Start;
   --           --  end if;
   --        end loop;
   --
   --     end set_Indices;
   --
   --
   --     the_Geometry.is_Transparent (False);
   --     the_Geometry.Vertices_are   (the_Vertices);
   --
   --     Self.Bounds := the_Bounds;
   --
   --     declare
   --        the_Primitive : constant Primitive.indexed.view
   --          := Primitive.indexed.new_Primitive (Primitive.line_Strip,
   --                                              the_Indices);
   --     begin
   --        the_Geometry.add (Primitive.view (the_Primitive));
   --     end;
   --
   --     return [1 => Geometry.view (the_Geometry)];
   --  end to_GL_Geometries;



   overriding
   procedure set_Bounds (Self : in out Item)
   is
      Heights      : height_Map_view renames Self.Heights;

      row_Count    : constant Index_t  := Heights'Length (1) - 1;
      col_Count    : constant Index_t  := Heights'Length (2) - 1;

      vertex_Count : constant Index_t  := Heights'Length (1) * Heights'Length (2);

      the_Sites    : aliased  Sites         := [1 .. vertex_Count => <>];
      the_Bounds   :          openGL.Bounds := null_Bounds;

   begin
      set_Sites:
      declare
         vert_Id          :          Index_t  := 0;
         the_height_Range : constant Vector_2 := height_Extent (Heights.all);
         Middle           : constant Real     :=   (the_height_Range (1) + the_height_Range (2))
                                                 / 2.0;
      begin
         for Row in 1 .. row_Count + 1
         loop
            for Col in 1 .. col_Count + 1
            loop
               vert_Id := vert_Id + 1;

               the_Sites (vert_Id) := [Real (Col)         - Real (col_Count) / 2.0 - 1.0,
                                       Heights (Row, Col) - Middle,
                                       Real (Row)         - Real (row_Count) / 2.0 - 1.0];

               the_Bounds.Box.Lower (1) := Real'Min (the_Bounds.Box.Lower (1),  the_Sites (vert_Id) (1));
               the_Bounds.Box.Lower (2) := Real'Min (the_Bounds.Box.Lower (2),  the_Sites (vert_Id) (2));
               the_Bounds.Box.Lower (3) := Real'Min (the_Bounds.Box.Lower (3),  the_Sites (vert_Id) (3));

               the_Bounds.Box.Upper (1) := Real'Max (the_Bounds.Box.Upper (1),  the_Sites (vert_Id) (1));
               the_Bounds.Box.Upper (2) := Real'Max (the_Bounds.Box.Upper (2),  the_Sites (vert_Id) (2));
               the_Bounds.Box.Upper (3) := Real'Max (the_Bounds.Box.Upper (3),  the_Sites (vert_Id) (3));

               the_Bounds.Ball := Real'Max (the_Bounds.Ball,
                                            abs (the_Sites (vert_Id)));
            end loop;
         end loop;

         the_Bounds.Ball := the_Bounds.Ball * 1.1;     -- TODO: Why the '* 1.1' ?
      end set_Sites;

      Self.Bounds := the_Bounds;
   end set_Bounds;


end openGL.Model.hex_grid;

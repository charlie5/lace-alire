generic
package any_Math.any_Geometry.any_d2.any_Hexagon with Pure
--
--   Models a regular, flat-topped hexagon.
--
--   https://en.wikipedia.org/wiki/Hexagon
--
--
--                 5   6
--                  ---
--                4/   \1
--                 \   /
--                  ---
--                 3   2
--
is
   -------------
   --- vertex_Id
   --
   subtype vertex_Id is any_Geometry.vertex_Id range 1 .. 6;

   function prior_Vertex (to_Vertex : in vertex_Id) return vertex_Id;
   function  next_Vertex (to_Vertex : in vertex_Id) return vertex_Id;



   --------
   --- Item
   --

   type Item is
      record
         circumRadius : Real;     -- 'R'
      end record;

   function R (Self : in Item) return Real;

   function Area         (Self : in Item) return Real;
   function Perimeter    (Self : in Item) return Real;

   function Angle        (Self : in Item;   at_Vertex : in vertex_Id) return Radians;

   function minimal_Diameter (Self : in Item) return Real;     -- 'd'
   function maximal_Diameter (Self : in Item) return Real;
   function D                (Self : in Item) return Real renames maximal_Diameter;

   function inRadius    (Self : in Item) return Real;          -- 'r'

   function side_Length (Self : in Item) return Real;
   function t           (Self : in Item) return Real renames side_Length;


   function Site (Self : in Item;   Id : vertex_Id) return any_d2.Site;



   --------
   --- Grid
   --
   -- Origin is at the top left corner.
   -- X increases to the right.
   -- Y increases downwards.
   --

   type Grid (Rows : Positive;
              Cols : Positive) is private;


   type Coordinates is
      record
         Row, Col: Positive;
      end record;


   function to_Grid (Rows, Cols   : in Positive;
                     circumRadius : in Real) return Grid;

   function hex_Center (Grid : in any_Hexagon.Grid;   Coords : in Coordinates) return any_d2.Site;
   --
   -- Returns the centre of the hexagon at the given co-ordinates.

   function vertex_Site (Self : in Grid;   hex_Id : in any_Hexagon.Coordinates;
                                           Which  : in any_Hexagon.vertex_Id) return any_d2.Site;



private

   type Grid (Rows : Positive;
              Cols : Positive) is
      record
         circumRadius : Real;
         Centers      : any_d2.Grid (1 .. Rows,
                                     1 .. Cols);
      end record;

end any_Math.any_Geometry.any_d2.any_Hexagon;

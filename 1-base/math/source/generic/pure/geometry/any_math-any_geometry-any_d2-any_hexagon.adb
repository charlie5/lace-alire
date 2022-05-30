package body any_Math.any_Geometry.any_d2.any_Hexagon
is

   -------------
   --- vertex_Id
   --

   function prior_Vertex (to_Vertex : in vertex_Id) return vertex_Id
   is
   begin
      if To_Vertex = 1
      then   return 6;
      else   return to_Vertex - 1;
      end if;
   end prior_Vertex;



   function next_Vertex (to_Vertex : in vertex_Id) return vertex_Id
   is
   begin
      if to_Vertex = 6
      then   return 1;
      else   return to_Vertex + 1;
      end if;
   end next_Vertex;



   --------
   --- Item
   --

   function R (Self : in Item) return Real
   is
   begin
      return Self.circumRadius;
   end R;



   function Area (Self : in Item) return Real
   is
   begin
      return 3.0 * R (Self) * inRadius (Self);
   end Area;



   function Perimeter (Self : in Item) return Real
   is
   begin
      return 6.0 * side_Length (Self);
   end Perimeter;



   function Angle (Self      : in Item      with Unreferenced;
                   at_Vertex : in vertex_Id with Unreferenced) return Radians
   is
   begin
      return to_Radians (120.0);
   end Angle;



   function minimal_Diameter (Self : in Item) return Real
   is
   begin
      return 2.0 * inRadius (Self);
   end minimal_Diameter;



   function maximal_Diameter (Self : in Item) return Real
   is
   begin
      return 2.0 * Self.circumRadius;
   end maximal_Diameter;



   function inRadius (Self : in Item) return Real
   is
      use Functions;
   begin
      return cos (to_Radians (30.0)) * R (Self);
   end inRadius;



   function side_Length (Self : in Item) return Real
   is
   begin
      return Self.circumRadius;
   end side_Length;


end any_Math.any_Geometry.any_d2.any_Hexagon;

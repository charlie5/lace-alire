generic
package any_Math.any_Geometry.any_d2.any_Hexagon with Pure
--
--   Models a regular, flat-topped hexagon.
--
--   https://en.wikipedia.org/wiki/Hexagon
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



private

end any_Math.any_Geometry.any_d2.any_Hexagon;

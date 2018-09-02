with Ada.Text_IO; use Ada.Text_IO;

package body sim_bits is

    R : integer := 1;
    ------------------------------------------

    function random (rndmax : in integer) return integer is
        K : constant integer := 2749;
    Begin
        R := integer ( (R + K) mod 16384);
        return (R mod rndmax);
    End random;


    function Get_String  return String is
        Line : String (1 .. 1_000);
        Last : Natural;
    begin
        Ada.Text_IO.Get_Line (Line, Last);
        return Line (1 .. Last);
    end Get_String;


    function Get_Integer return Integer is
        S : constant String := Get_String;
    begin
        return Integer'Value (S);
        --  may raise exception Constraint_Error if value entered is not a well-formed integer
    end Get_Integer;

end sim_bits;

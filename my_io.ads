with Ada.Text_IO;    use Ada.Text_IO;

package my_io is

    SCREEN_DEPTH : constant INTEGER := 24;
    SCREEN_WIDTH : constant INTEGER := 79;

    type column_range is range 0 .. SCREEN_WIDTH;
    type row_range is range 0 .. SCREEN_DEPTH;

    Task Display is
        entry CLS;
        entry DisplayString (s      : in string;   x : in row_range; y : in column_range);
        entry DisplayChar  (c       : in character;x : in row_range; y : in column_range);
        entry DisplayInt   (i, len  : in integer;  x : in row_range; y : in column_range);
        entry DisplayTime  (h, m, s : in integer;  x : in row_range; y : in column_range);
        entry Pause        (s       : in string;   x : in row_range; y : in column_range);
    end Display;

    Task Input is
        entry InputString (s : out string;  x : in row_range; y : in column_range);
        entry InputChar  (c : in character; x : in row_range; y : in column_range);
        entry InputInt   (i : in integer;  x : in row_range; y : in column_range);
    end Input;

    Task FileInOut is
        entry Create  (file : in out Ada.Text_IO.file_type; s : in string);
        entry Put_Str (file : in Ada.Text_IO.file_type; s : in string);
        entry Put_Int (file : in Ada.Text_IO.file_type; i, len : in integer);
        entry Put_Line (file : in Ada.Text_IO.file_type);
        entry Close   (file : in out Ada.Text_IO.file_type);
    end FileInOut;

end my_io;

with Ada.Text_IO; use Ada.Text_IO;
with sim_bits;

package my_io is

    SCREEN_DEPTH : constant INTEGER := 24;
    SCREEN_WIDTH : constant INTEGER := 79;

    Task Display is
        entry CLS;
        entry DisplayString (s       : in string;   row : in Positive_Count; col : in Positive_Count);
        entry DisplayChar   (c       : in character;row : in Positive_Count; col : in Positive_Count);
        entry DisplayInt    (i, len  : in integer;  row : in Positive_Count; col : in Positive_Count);
        entry DisplayTime   (h, m, s : in integer;  row : in Positive_Count; col : in Positive_Count);
        entry Pause         (s       : in string;   row : in Positive_Count; col : in Positive_Count);
    end Display;

    Task Input is
        entry InputString (s : out string;  row : in Positive_Count; col : in Positive_Count);
    end Input;

    Task FileInOut is
        entry Create   (file : in out File_Type; s : in string);
        entry Put_Str  (file : in File_Type; s : in string);
        entry Put_Int  (file : in File_Type; i, len : in integer);
        entry Put_Line (file : in File_Type);
        entry Close    (file : in out File_Type);
    end FileInOut;

end my_io;

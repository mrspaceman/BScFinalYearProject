with ATMTypes:       use ATMTypes;
with text_io;

package my_io is

  SCREEN_DEPTH : constant INTEGER := 23;
  SCREEN_WIDTH : constant INTEGER := 79;

  Task Display is
    entry CLS;
    entry DisplayString(s : in string;       x: in row_range; y : in column_range);
    entry DisplayChar  (c : in character;    x: in row_range; y : in column_range);
    entry DisplayInt   (i,len : in integer;  x: in row_range; y : in column_range);
    entry DisplayTime  (h,m,s : in integer;  x: in row_range; y : in column_range);
    entry Pause        (s : in string;       x: in row_range; y : in column_range);
  end Display;

  Task Input is
    entry InputString(s : out string;  x: in row_range; y : in column_range);
    entry InputChar  (c : in character;x: in row_range; y : in column_range);
    entry InputInt   (i : in integer;  x: in row_range; y : in column_range);
  end Input;

  Task FileInOut is
    entry Create  (file : in out text_io.file_type; s : in string);
    entry Put_Str (file : in text_io.file_type; s : in string);
    entry Put_Int (file : in text_io.file_type; i : in integer);
    entry Put_Line(file : in text_io.file_type);
    entry Close   (file : in out text_io.file_type);
  end FileInOut;

end my_io;

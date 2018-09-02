with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;


package body my_io is

    -- These two variables are used to reset the cursor position
    -- after displaying the line
    oldx  : Positive_Count := 1;
    oldy  : Positive_Count := 1;
    dummy : character := ' ';

    ------------------------------------------------------------------------------

    Task body Display is
    begin
        -- This procedure handles all the messages to the screen;
        loop
            select

                accept CLS do
                    -- Clears the screen
                    Ada.Text_IO.Put (ASCII.ESC & "[2J");
                    oldx := 1;
                    oldy := 1;
                end CLS;

            or -- select

                accept DisplayString (s : in string; x : in Positive_Count; y : in Positive_Count) do
                    -- Displays strings to the screen at the correct location.
                    Ada.Text_IO.Set_Col (x);
                    Ada.Text_IO.Set_Line (y);
                    Ada.Text_IO.Put_Line (s);
                end DisplayString;

            or -- select

                accept DisplayChar (c : in character; x : in Positive_Count; y : in Positive_Count) do
                    -- Displays characters to the screen at the correct location.
                    Ada.Text_IO.Set_Col (x);
                    Ada.Text_IO.Set_Line (y);
                    Ada.Text_IO.Put (c);
                end DisplayChar;

            or -- select

                accept DisplayInt (i, len : in integer; x : in Positive_Count; y : in Positive_Count) do
                    -- Displays integers to the screen at the correct location.
                    Ada.Text_IO.Set_Col (x);
                    Ada.Text_IO.Set_Line (y);
                    Ada.Integer_Text_IO.Put (i);                          -- i is the number, len is the length - to avoid spurios spaces.
                end DisplayInt;

            or -- select

                accept DisplayTime (h, m, s : in integer; x : in Positive_Count; y : in Positive_Count) do
                    -- Displays the time to the screen at the correct location.
                    oldx := Ada.Text_IO.Col;
                    oldy := Ada.Text_IO.Line;
                    Ada.Text_IO.Set_Col (x);                  -- Moves the cursor to the correct position that the clock will be displayed.
                    Ada.Text_IO.Set_Line (y);


                    if (h < 10) then                          -- if the hours are less than ten a zero is put in front of the digit
                        Ada.Text_IO.Put ("0");                         -- else the hours are displayed as input - needed to achieve the
                        Ada.Integer_Text_IO.Put (h);                      -- correct format
                    else
                        Ada.Integer_Text_IO.Put (h);
                    end if;

                    Ada.Text_IO.Put (":");

                    if (m < 10) then                          -- if the minutes are less than ten a zero is put in front of the
                        Ada.Text_IO.Put ("0");                         -- digit else the minutes are displayed as input - needed to achieve the
                        Ada.Integer_Text_IO.Put (m);                      -- correct format
                    else
                        Ada.Integer_Text_IO.Put (m);
                    end if;

                    Ada.Text_IO.Put (":");

                    if (m < 10) then                          -- if the seconds are less than ten a zero is put in front of the
                        Ada.Text_IO.Put ("0");                         -- digit else the seconds are displayed as input - needed to achieve the
                        Ada.Integer_Text_IO.Put (s);                      -- correct format
                    else
                        Ada.Integer_Text_IO.Put (s);
                    end if;

                    Ada.Text_IO.Set_Col (x);
                    Ada.Text_IO.Set_Line (y);

                end DisplayTime;

            or -- select

                accept Pause (s : in string; x : in Positive_Count; y : in Positive_Count) do
                    Ada.Text_IO.Set_Col (x);
                    Ada.Text_IO.Set_Line (y);
                    Ada.Text_IO.Put ( s);
                    Ada.Text_IO.Get (dummy);
                end Pause;

            or -- select

                terminate;

            end select;
        end loop;

    exception
        when tasking_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put("Tasking Error in Display");
        when program_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Program Error in Display");
        when storage_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Storage Error in Display");
        when numeric_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Numeric Error in Display");
    --    when constraint_error   => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Constraint Error in Display");
        when others             => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Other Error in Display");

    end Display;

    ------------------------------------------------------------------------------

    Task body Input is
    begin
        -- This procedure handles all the messages to the screen;

        loop
            select

                accept InputString (s : out string;  x : in Positive_Count; y : in Positive_Count) do
                    -- Inputs strings from the keyboard at the correct screen location.
                    Ada.Text_IO.Set_Col (x);
                    Ada.Text_IO.Set_Line (y);
                    Ada.Text_IO.get (s);
                end InputString;

            or

                terminate;

            end select;
        end loop;

    exception
        when tasking_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Tasking Error in Input");
        when program_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Program Error in Input");
        when storage_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Storage Error in Input");
        when numeric_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Numeric Error in Input");
  --      when constraint_error   => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Constraint Error in Input ");
        when others             => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Other Error in Input");

    end Input;

-------------------------------------------------------------------------------

    Task body FileInOut is
    begin
        loop
            select

                accept Create (file : in out File_Type ; s : in string) do
                    Ada.Text_IO.create (file, Out_File, s);
                end Create;

            or

                accept Put_Str (file : in File_Type; s : in string) do
                    Ada.Text_IO.put (file,  s);
                end Put_Str;

            or

                accept Put_Int (file : in File_Type; i,len : in integer) do
                    Ada.Text_IO.put (file,  Integer'Image(i));
                end Put_Int;

            or

                accept Put_Line (file : in File_Type) do
                    Ada.Text_IO.New_Line ( file);
                end Put_Line;

            or

                accept Close (file : in out File_Type) do
                    Ada.Text_IO.close ( file);
                end Close;

            or

                terminate;

            end select;
        end loop;


    exception
        when tasking_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Tasking Error in FileInOut");
        when program_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Program Error in FileInOut");
        when storage_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Storage Error in FileInOut");
        when numeric_error      => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Numeric Error in FileInOut");
    --    when constraint_error   => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Constraint Error in FileInOut");
        when others             => Ada.Text_IO.Set_Col (24);Ada.Text_IO.Set_Line (1); Ada.Text_IO.Put ("Other Error in FileInOut");

    end FileInOut;

    ------------------------------------------------------------------------------

end my_io;

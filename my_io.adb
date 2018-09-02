with Ada.Text_IO;    use Ada.Text_IO;
--package Ada.Integer_Text_IO is new Ada.Text_IO.Integer_IO (Integer);
with Ada.Integer_Text_IO;
--with Ada.tty;
--with Ada.cursor;

package body my_io is

   -- These two variables are used to reset the cursor position
   -- after displaying the line
   oldx : row_range := 0;
   oldy : column_range :=0;
   dummy: character := ' ';

   ------------------------------------------------------------------------------

   Task body Display is
      use Ada.Text_IO;
   begin
      -- This procedure handles all the messages to the screen;
      loop
         select

            accept CLS do
               -- Clears the screen
               tty.clear_screen;
               oldx := 0;
               oldy := 0;
            end CLS;

         or -- select

            accept DisplayString(s : in string; x: in row_range; y : in column_range) do
               -- Displays strings to the screen at the correct location.
               tty.put(x,y,s,false,false,false,false);
            end DisplayString;

         or -- select

            accept DisplayChar(c : in character; x: in row_range; y : in column_range) do
               -- Displays characters to the screen at the correct location.
               tty.put(x,y,c,false,false,false,false);
            end DisplayChar;

         or -- select

            accept DisplayInt(i,len : in integer; x: in row_range; y : in column_range) do
               -- Displays integers to the screen at the correct location.
               cursor.move(x,y,0);
               int_io.put(i);                          -- i is the number, len is the length - to avoid spurios spaces.
            end DisplayInt;

         or -- select

            accept DisplayTime(h,m,s : in integer; x: in row_range; y : in column_range) do
               -- Displays the time to the screen at the correct location.
               cursor.get_position(oldx,oldy,0);
               cursor.move(x,y,0);                     -- Moves the cursor to the correct position that the clock will be displayed.

               if (h<10) then                          -- if the hours are less than ten a zero is put in front of the digit
                  tty.put("0");                         -- else the hours are displayed as input - needed to achieve the
                  int_io.put(h,1);                      -- correct format
               else
                  int_io.put(h,2);
               end if;

               text_io.put(":");

               if (m<10) then                          -- if the minutes are less than ten a zero is put in front of the
                  tty.put("0");                         -- digit else the minutes are displayed as input - needed to achieve the
                  int_io.put(m,1);                      -- correct format
               else
                  int_io.put(m,2);
               end if;

               text_io.put(":");

               if (m<10) then                          -- if the seconds are less than ten a zero is put in front of the
                  tty.put("0");                         -- digit else the seconds are displayed as input - needed to achieve the
                  int_io.put(s,1);                      -- correct format
               else
                  int_io.put(s,2);
               end if;

               cursor.move(oldx,oldy,0);

            end DisplayTime;

         or -- select

            accept Pause(s : in string; x: in row_range; y : in column_range) do
               tty.put(x,y,s,false,false,false,false);
               Input.InputChar(dummy,0,0);
            end Pause;

         or -- select

            terminate;

         end select;
      end loop;

   exception
      when tasking_error      => cursor.move(24,1,0);text_io.put("Tasking Error in Display");
      when program_error      => cursor.move(24,1,0);text_io.put("Program Error in Display");
      when storage_error      => cursor.move(24,1,0);text_io.put("Storage Error in Display");
      when numeric_error      => cursor.move(24,1,0);text_io.put("Numeric Error in Display");
      when constraint_error   => cursor.move(24,1,0);text_io.put("Constraint Error in Display");
      when others             => cursor.move(24,1,0);text_io.put("Other Error in Display");

   end Display;

   ------------------------------------------------------------------------------

   Task body Input is
      use text_io;
      temp : natural;
   begin
      -- This procedure handles all the messages to the screen;

      loop
         select

            accept InputString(s : out string;  x: in row_range; y : in column_range) do
               -- Inputs strings from the keyboard at the correct screen location.
               cursor.move(x,y,0);
               tty.get(s,temp);
            end InputString;

         or

            accept InputChar(c : in character;x: in row_range; y : in column_range) do
               -- Inputs characters from the keyboard at the correct screen location.
               cursor.move(x,y,0);
               c := tty.get(false,fasle,false);
            end InputChar;

         or

            accept InputInt(i : in integer;  x: in row_range; y : in column_range) do
               -- Inputs integers from the keyboard at the correct screen location.
               cursor.move(x,y,0);
               tty.get(i,temp);
            end InputInt;

         or

            terminate;

         end select;
      end loop;

   exception
      when tasking_error      => cursor.move(24,1,0);text_io.put("Tasking Error in Input");
      when program_error      => cursor.move(24,1,0);text_io.put("Program Error in Input");
      when storage_error      => cursor.move(24,1,0);text_io.put("Storage Error in Input");
      when numeric_error      => cursor.move(24,1,0);text_io.put("Numeric Error in Input");
      when constraint_error   => cursor.move(24,1,0);text_io.put("Constraint Error in Input ");
      when others             => cursor.move(24,1,0);text_io.put("Other Error in Input");

   end Input;

   ------------------------------------------------------------------------------


   ------------------------------------------------------------------------------

   Task body FileInOut is
   begin
      loop
         select

            accept Create(file : in out text_io.file_type; s : in string) do
               text_io.create(Mode => Rewrite);
            end Create;

         or

            accept Put_Str(file : in text_io.file_type; s : in string) do
               text_io.put(FILE=>file, ITEM=>s);
            end Put_Str;

         or

            accept Put_Int(file : in text_io.file_type; i : in integer) do
               text_io.put(FILE=>file, ITEM=>i, WIDTH=>len);
            end Put_Int;

         or

            accept Put_Line(file : in text_io.file_type) do
               text_io.New_Line(FILE=>file);
            end Put_Line;

         or

            accept Close(file : in out text_io.file_type) do
               text_io.close(FILE=>file);
            end Close;

         or

            terminate;

         end select;
      end loop;


   exception
      when tasking_error      => cursor.move(24,1,0);text_io.put("Tasking Error in FileInOut");
      when program_error      => cursor.move(24,1,0);text_io.put("Program Error in FileInOut");
      when storage_error      => cursor.move(24,1,0);text_io.put("Storage Error in FileInOut");
      when numeric_error      => cursor.move(24,1,0);text_io.put("Numeric Error in FileInOut");
      when constraint_error   => cursor.move(24,1,0);text_io.put("Constraint Error in FileInOut");
      when others             => cursor.move(24,1,0);text_io.put("Other Error in FileInOut");

   end FileInOut;

   ------------------------------------------------------------------------------

   end my_io;

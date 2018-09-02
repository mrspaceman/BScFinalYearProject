with ATMTypes;       use ATMTypes;
with my_io;

package body Output_Buffer is

task body ATMBuffer is

  Buffer      : array(1..MaxBufferSize) of ATMCell;
  BufferFirst : INTEGER := 1; -- Next Empty slot for adding into buffer
  BufferLast  : INTEGER := 1; -- First cell to send from buffer
begin
  BufferFirst := 1;
  BufferLast  := 1;
  loop
    select
      accept StopNow do
        null;
      end StopNow;
      exit;

    or  --select

      accept AddToBuffer(CellToAdd: in ATMCell) do
        if ((BufferFirst=MaxBufferSize) AND (BufferLast=1)) OR (BufferFirst+1=BufferLast) then
          null; -- buffer full
        else
          Buffer(BufferFirst) := CellToAdd;
          BufferFirst := BufferFirst+1;

          if BufferFirst > MaxBufferSize then
            BufferFirst := 1;
          end if;
        end if;
      end AddToBuffer;

    or  --select

      accept RemoveFromBuffer(BufferEmpty: in out integer; CellToRemove: out ATMCell) do
        -- if buffer is not empty then take first cell and send it to next switch
        if BufferFirst /= BufferLast then
          CellToRemove := Buffer(BufferLast);
          BufferEmpty  := 1;
          BufferLast   := BufferLast+1;
          if BufferLast > MaxBufferSize then
            BufferLast :=1;
          end if;
        else
          BufferEmpty             :=0;
          CellToRemove.CellHeader :=0;
          CellToRemove.CellBody   :=0;
        end if;
      end RemoveFromBuffer;

    or  --select

      accept CountCells(NoOfCells: out INTEGER) do
        if BufferFirst>BufferLast then
          NoOfCells := BufferFirst - BufferLast;
        else
          if BufferFirst<BufferLast then
            NoOfCells := (MaxBufferSize-BufferLast)+BufferFirst;
          else
            NoOfCells := 0;
          end if;
        end if;
      end CountCells;

    end select;

  end loop;

  exception
    when tasking_error      => my_io.display.Pause("Tasking Error in Buffer    ",   24, 1);
    when program_error      => my_io.display.Pause("Program Error in Buffer    ",   24, 1);
    when storage_error      => my_io.display.Pause("Storage Error in Buffer    ",   24, 1);
    when numeric_error      => my_io.display.Pause("Numeric Error in Buffer    ",   24, 1);
--    when constraint_error   => my_io.display.Pause("Constraint Error in Buffer ",   24, 1);
    when others             => my_io.display.Pause("Other Error in Buffer      ",   24, 1);

  end ATMBuffer;

end Output_Buffer;

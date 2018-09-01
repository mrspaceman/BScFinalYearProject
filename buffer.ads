with ATMTypes:       use ATMTypes;

package Output_Buffer is

  Task type ATMBuffer is
    entry StopNow;
    entry AddToBuffer(CellToAdd : in ATMCell);
    entry RemoveFromBuffer(BufferEmpty: in out integer; CellToRemove: out ATMCell);
    entry CountCells(NoOfCells: out INTEGER);
  end ATMElement;

end Output_Buffer;

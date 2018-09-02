with ATMTypes; use ATMtypes;

package switch_element is

  Task type ATMElement is
    entry Initialise(x : in integer);
    entry StopNow;
    entry CellArrival(Cell : in ATMCell);
    entry CellDeparture(CellReady: in out integer; Port: integer; Cell : in out ATMCell);
  end ATMElement;

end switch_element;

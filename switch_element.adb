with ATMTypes;       use ATMTypes;
with my_io;          use my_io;
with output_buffer;  use output_buffer;
with sim_bits;       use sim_bits;


package body switch_element is

    type RouteStruct is
        record
            OutPort      : PortType := 0;
            HeaderRecord : ATMHeader;
        end record;

    ------------------------------------------------------------------------------

    task body ATMElement is
    --                       InPort ,     InCellHeader
        RouteTable     : array (1 .. NoOfPorts , 1 .. NoRoutes) of RouteStruct;
        CellIn, CellOut : ATMCell := (0, 0, 0);
        BufferOut      : array (1 .. NoOfPorts) of ATMBuffer;

        BufferEmpty, NoOfCellsInBuffer, NodeNumber, x, y : integer := 0;

        ------------------------------------------------------------------------------

        function route (CellToRoute : in ATMCell) return ATMCell is
            NewCell : ATMCell;
        begin
            NewCell.Port       := RouteTable (CellToRoute.Port, CellToRoute.CellHeader).OutPort;
            NewCell.CellHeader := RouteTable (CellToRoute.Port, CellToRoute.CellHeader).HeaderRecord;
            NewCell.CellBody   := CellToRoute.CellBody;
            return NewCell;
        end route;

        ------------------------------------------------------------------------------
    begin
        for x in 1 .. NoRoutes loop
            display.DisplayLogMsg ("setup_file() switch element initialise route " & Integer'Image (x) );
            RouteTable (1, x).OutPort   := 2;
            RouteTable (2, x).OutPort   := 1;
            RouteTable (1, x).HeaderRecord := random (NoRoutes)+1;
            RouteTable (2, x).HeaderRecord := random (NoRoutes)+1;
        end loop;

        accept initialise (x : in integer) do
            display.DisplayLogMsg ("setup_file() initialise node " & Integer'Image (x) );
            NodeNumber := x;
            my_io.fileInOut.put_line (fd);
            my_io.fileInOut.put_str (fd, "Routing Transformation Table for Switch ");
            my_io.fileInOut.put_int (fd, NodeNumber, 3);
            my_io.fileInOut.put_line (fd);
            my_io.fileInOut.put_str (fd, "  Cell In                  Cell Out ");
            my_io.fileInOut.put_line (fd);
            my_io.fileInOut.put_str (fd, "Port   Header            Port   Header");
            my_io.fileInOut.put_line (fd);
            for x in 1 .. NoOfPorts loop
                for y in 1 .. NoRoutes loop
                    display.DisplayLogMsg ("setup_file() initialise loop x=" & Integer'Image (x) & " y=" & Integer'Image (y) );
                    my_io.fileInOut.put_str (fd, " ");
                    my_io.fileInOut.put_int (fd, x, 2);
                    my_io.fileInOut.put_str (fd, "     ");
                    my_io.fileInOut.put_int (fd, y, 2);
                    my_io.fileInOut.put_str (fd, "       -        ");
                    my_io.fileInOut.put_int (fd, RouteTable (x, y).OutPort, 2);
                    my_io.fileInOut.put_str (fd, "     ");
                    my_io.fileInOut.put_int (fd, RouteTable (x, y).HeaderRecord, 2);
                    my_io.fileInOut.put_line (fd);
                end loop;
            end loop;
            display.DisplayLogMsg ("setup_file() initialise end ");
        end initialise;

        loop
            select
                accept StopNow do
                    for x in 1 .. NoOfPorts loop
                        BufferOut (x).StopNow;
                    end loop;
                end StopNow;
                exit;

            or -- select

                accept CellArrival (Cell : in ATMCell) do

                    my_io.fileInOut.put_line (fd);
                    my_io.fileInOut.put_str (fd, "        New Cell into switch ");
                    my_io.fileInOut.put_int (fd, Cell.Port, 2);
                    my_io.fileInOut.put_str (fd, "           ");
                    my_io.fileInOut.put_int (fd, Cell.CellHeader, 2);

                    CellIn.Port      := Cell.Port;
                    CellIn.CellHeader := Cell.CellHeader;
                    CellIn.CellBody  := Cell.CellBody;
                    CellOut          := route (CellIn);

                    my_io.fileInOut.put_str (fd, "           ");
                    my_io.fileInOut.put_int (fd, CellOut.Port, 2);
                    my_io.fileInOut.put_str (fd, "           ");
                    my_io.fileInOut.put_int (fd, CellOut.CellHeader, 2);

                    BufferOut (CellOut.Port).AddToBuffer (CellOut);
                    my_io.fileInOut.put_str (fd, "           ");
                    BufferOut (CellOut.Port).CountCells (NoOfCellsInBuffer);
                    my_io.fileInOut.put_int (fd, NoOfCellsInBuffer, 2);

                end CellArrival;

            or -- select

                accept CellDeparture (CellReady : in out integer; Port : integer; Cell : in out ATMCell) do

                    BufferOut (Port).RemoveFromBuffer (BufferEmpty, CellOut);
                    Cell := CellOut;

                    if BufferEmpty = 0 then
                        CellReady := 0;
                    else
                        my_io.fileInOut.put_line (fd);
                        my_io.fileInOut.put_str (fd, "        Cell leaves switch     ");
                        my_io.fileInOut.put_int (fd, CellOut.Port, 2);
                        my_io.fileInOut.put_str (fd, "           ");
                        my_io.fileInOut.put_int (fd, CellOut.CellHeader, 2);
                        my_io.fileInOut.put_str (fd, "                                          ");
                        BufferOut (CellOut.Port).CountCells (NoOfCellsInBuffer);
                        my_io.fileInOut.put_int (fd, NoOfCellsInBuffer, 2);
                    end if;

                end CellDeparture;

            end select;

        end loop;

    exception
        when tasking_error      => my_io.display.Pause ("Tasking Error in ATMElement    ",   24, 1);
        when program_error      => my_io.display.Pause ("Program Error in ATMElement    ",   24, 1);
        when storage_error      => my_io.display.Pause ("Storage Error in ATMElement    ",   24, 1);
        when numeric_error      => my_io.display.Pause ("Numeric Error in ATMElement    ",   24, 1);
            --   when constraint_error   => my_io.display.Pause("Constraint Error in ATMElement ",   24, 1);
        when others             => my_io.display.Pause ("Other Error in ATMElement      ",   24, 1);

    end ATMElement;

    ------------------------------------------------------------------------------

end switch_element;

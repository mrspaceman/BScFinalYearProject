with ATMTypes;       use ATMTypes;
with switch_element; use switch_element;
with my_io;          use my_io;
with sim_bits;       use sim_bits;

Procedure Simulate is
    NetworkNodes    : array (1 .. MaxNumberOfNodes) of ATMElement;
    CellsInTransit  : array (1 .. (NoOfPorts * MaxNumberOfNodes)) of TransitCell;
    links           : array (1 .. MaxNumberOfNodes, 1 .. NoOfPorts) of LinkStruct;
    NewCell         : ATMCell := (0, 0, 0);
    dummy           : character := 'L';

    nodeNumber, nodePort, cycleNumber, x, y         : integer := 1;
    cellReady, nextFreeTransit, TransitCellToSwitch : integer := 1;
    ------------------------------------------------------------------------------

    Procedure setup_screen is
    Begin
        my_io.display.cls;
        my_io.display.displayString (Title, 1, 40-(Title'LENGTH / 2));  -- Centres Title on Screen
        my_io.display.displayString (Author, 1, (79 - Author'LENGTH)); -- Right Justifies Author on screen
        my_io.display.displayString ("Cycle Number", 5, 10);

    End setup_screen;

    Procedure setup_file is
    Begin
        my_io.fileInOut.create  (fd, datafilename);
        my_io.fileInOut.put_str (fd, Title);
        my_io.fileInOut.put_str (fd, "    Version:");
        my_io.fileInOut.put_str (fd, Version);
        my_io.fileInOut.put_str (fd, "    By:");
        my_io.fileInOut.put_str (fd, Author);
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_str (fd, "Number of Switches:");
        my_io.fileInOut.put_int (fd, MaxNumberOfNodes, 2);
        my_io.fileInOut.put_str (fd, "      ");
        my_io.fileInOut.put_str (fd, "Number of Ports per Switch:");
        my_io.fileInOut.put_int (fd, NoOfPorts, 2);
        my_io.fileInOut.put_str (fd, "      ");
        my_io.fileInOut.put_str (fd, "Buffer Size:");
        my_io.fileInOut.put_int (fd, MaxBufferSize, 2);
        my_io.fileInOut.put_str (fd, "      ");
        my_io.fileInOut.put_str (fd, "Number of Routes per Switch:");
        my_io.fileInOut.put_int (fd, NoRoutes, 2);
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_str (fd, "    Switch to Switch Links");
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_str (fd, "From Switch, Port    to Switch, Port");

        for x in 1 .. MaxNumberOfNodes loop
            for y in 1 .. NoOfPorts loop
                my_io.fileInOut.put_line (fd);
                my_io.fileInOut.put_str (fd, "   ");
                my_io.fileInOut.put_int (fd, x, 2);
                my_io.fileInOut.put_str (fd, ",");
                my_io.fileInOut.put_int (fd, y, 2);
                my_io.fileInOut.put_str (fd, "         ");
                my_io.fileInOut.put_int (fd, links (x, y).Switch, 2);
                my_io.fileInOut.put_str (fd, ",");
                my_io.fileInOut.put_int (fd, links (x, y).Port, 2);
            end loop;
        end loop;

        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_line (fd);
        for x in 1 .. MaxNumberOfNodes loop
            NetworkNodes (nodeNumber).Initialise (nodeNumber);
        end loop;
        my_io.fileInOut.put_line (fd); my_io.fileInOut.put_line (fd);

        my_io.fileInOut.put_str (fd, " Cycle No    Switch No    InPort    Cell Header    Outport    Cell Header    No. Cells");
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_str (fd, "                                        In                    After Route    In Buffer");
    End setup_file;

    Function GenerateNewCell (port : INTEGER) return ATMCell is
        GeneratedCell : ATMCell;
    Begin
        GeneratedCell.Port       := port;
        GeneratedCell.CellHeader := random (noRoutes)+1;
        GeneratedCell.CellBody   := random (99)+1;
        return GeneratedCell;
    End GenerateNewCell;

Begin

    for nodeNumber in 1 .. MaxNumberOfNodes loop
        for y in 1 .. NoOfPorts loop
            links (nodeNumber, y).Port  := random (NoOfPorts)+1;
            links (nodeNumber, y).Switch := random (MaxNumberOfNodes)+1;
        end loop;
    end loop;

    setup_screen;
    setup_file;

    dummy := 'l';
    cycleNumber := 1;

    loop   -- One loop is a single time cycle

        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_str (fd, "   ");
        my_io.fileInOut.put_int (fd, cycleNumber, 3);
        my_io.display.displayint (cycleNumber, 3, 5, 24);

        for nodeNumber in 1 .. MaxNumberOfNodes loop
            my_io.fileInOut.put_line (fd);
            my_io.fileInOut.put_str (fd, "                ");
            my_io.fileInOut.put_int (fd, nodeNumber, 2);
            for nodePort in 1 .. NoOfPorts loop

                if Random (2) = 0 then
                    NetworkNodes (nodeNumber).CellDeparture (cellReady,nodePort,NewCell);
                else
                    CellReady := 0;
                end if;

                if CellReady = 1 then
                    CellsInTransit (nextFreeTransit).CellHeader := NewCell.CellHeader;
                    CellsInTransit (nextFreeTransit).CellBody  := NewCell.CellBody;
                    CellsInTransit (nextFreeTransit).ToPort    := links (nodeNumber,nodePort).Port;
                    CellsInTransit (nextFreeTransit).ToSwitch  := links (nodeNumber,nodePort).Switch;
                end if;

                TransitCellToSwitch := 0;
                for x in 1 .. nextFreeTransit loop
                    if (CellsInTransit (x).ToSwitch = nodeNumber) AND (CellsInTransit (x).ToPort = nodePort) then
                        TransitCellToSwitch := x;
                        NewCell.CellHeader  := CellsInTransit (x).CellHeader;
                        NewCell.CellBody    := CellsInTransit (x).CellBody;
                        NewCell.Port        := nodePort;
                        exit;
                    end if;
                end loop;
                if TransitCellToSwitch > 0 then
                    NetworkNodes (nodeNumber).CellArrival (NewCell);
                    for x in TransitCellToSwitch .. nextFreeTransit loop
                        CellsInTransit (x) := CellsInTransit (x + 1);
                    end loop;
                else
                    NewCell := GenerateNewCell (nodePort);
                    NetworkNodes (nodeNumber).CellArrival (NewCell);
                end if;
            end loop;
            my_io.fileInOut.put_line (fd);
            my_io.fileInOut.put_str (fd, "--------------------------------------------------------------------------------");

        end loop;
        my_io.fileInOut.put_line (fd);
        my_io.fileInOut.put_str (fd, "==================================================================================");
        my_io.fileInOut.put_line (fd);

        cycleNumber := cycleNumber + 1;
        if cycleNumber > NoOfLoops then
            exit;
        end if;
    end loop;

    my_io.fileInOut.put_line (fd);
    my_io.fileInOut.put_str (fd, "End Of File");
    my_io.fileInOut.close (fd);

    for count in 1 .. MaxNumberOfNodes loop
        NetworkNodes (count).StopNow;
    end loop;

exception
    when tasking_error      => my_io.display.Pause ("Tasking Error in Simulate    ",   24, 1);
    when program_error      => my_io.display.Pause ("Program Error in Simulate    ",   24, 1);
    when storage_error      => my_io.display.Pause ("Storage Error in Simulate    ",   24, 1);
    when numeric_error      => my_io.display.Pause ("Numeric Error in Simulate    ",   24, 1);
 --   when constraint_error   => my_io.display.Pause ("Constraint Error in Simulate ",   24, 1);
    when others             => my_io.display.Pause ("Other Error in Simulate      ",   24, 1);

End simulate;

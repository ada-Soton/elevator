--  Copyright (c) 2020, 2023 University of Southampton.
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.

--  This package defines the type for button types.
--
--  @author: htson
--  @version: 1.0
with Floor_Package; use Floor_Package;

package body User_Package with SPARK_Mode is

   procedure user is
      choice : Character := 'x';
      floor : Integer;

      procedure ControlPanel is
      begin
         Put_Line( "=== Control Panel ===" ) ;
         Put_Line( "(f) Press a Floor button" ) ;
         Put_Line( "(u) Press an Up button" ) ;
         Put_Line( "(d) Press a Down button" ) ;
         Put_Line( "(e) End user input" ) ;
         Put_Line( "======================" ) ;
      end ControlPanel;
   begin
      while choice /= 'e' loop
         ControlPanel;
         Put( "Please enter your selection? " ) ;
         Get(choice);
         case choice is
         when 'f' =>
            Put( "    Which floor button? " ) ;
            Get(floor);
            if 0 <= floor and floor <= TOP_FLOOR then
               if floor_buttons_array(floor) then
                  Put_Line(" Floor button is already pressed ");
               else
                  FloorButtonPresses(floor);
                  Put("=User= Presses FLOOR button ");
                  Put(floor);
                  Put_Line("");
                  Put;
               end if;
            else
              Put_Line("Invalid floor button");
            end if;
         when 'u' =>
            Put( "    Which up button? " ) ;
            Get(floor);
            if 0 <= floor and floor < TOP_FLOOR then
               if up_buttons_array(floor) then
                  Put_Line(" Up button is already pressed ");
               else
                  UpButtonPresses(floor);
                  Put("=User= Presses UP button ");
                  Put(floor);
                  Put_Line("");
                  Put;
               end if;
            else
              Put_Line("Invalid up button");
            end if;
         when 'd' =>
            Put( "    Which down button? " ) ;
            Get(floor);
            if 0 < floor and floor <= TOP_FLOOR then
               if down_buttons_array(floor) then
                  Put_Line(" Down button is already pressed ");
               else
                  DownButtonPresses(floor);
                  Put("=User= Presses DOWN button ");
                  Put(floor);
                  Put_Line("");
                  Put;
               end if;
            else
              Put_Line("Invalid down button");
            end if;
         when others => null;
         end case;
      end loop;
   end user;

end User_Package;

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
with Button_Package; use Button_Package;
with Door_Package; use Door_Package;
with Direction_Package; use Direction_Package;
with Floor_Package; use Floor_Package;
with Motor_Package; use Motor_Package;

package body Controller_Package with SPARK_Mode is
   
   function HasFloorRequest(buttons_array : FLOOR_BUTTONS_ARRAY_Type;
                       first : Integer; last : Integer) return Boolean
     with Post => HasFloorRequest'Result = (for some I in buttons_array'Range =>
       first <= I and I <= last and buttons_array(I) = TRUE)
   is
   begin
      for I in buttons_array'Range loop
         if first <= I and I <= last and buttons_array(I) = TRUE then
            return TRUE;
         end if;
      end loop;
      return FALSE;
   end HasFloorRequest;

   function HasUpRequest(buttons_array : Up_BUTTONS_ARRAY_Type;
                       first : Integer; last : Integer) return Boolean
     with Post => HasUpRequest'Result = (for some I in buttons_array'Range =>
       first <= I and I <= last and buttons_array(I) = TRUE)
   is
   begin
      for I in buttons_array'Range loop
         if first <= I and I <= last and buttons_array(I) = TRUE then
            return TRUE;
         end if;
      end loop;
      return FALSE;
   end HasUpRequest;

   function HasDownRequest(buttons_array : DOWN_BUTTONS_ARRAY_Type;
                       first : Integer; last : Integer) return Boolean
     with Post => HasDownRequest'Result = (for some I in buttons_array'Range =>
       first <= I and I <= last and buttons_array(I) = TRUE)
   is
   begin
      for I in buttons_array'Range loop
         if first <= I and I <= last and buttons_array(I) = TRUE then
            return TRUE;
         end if;
      end loop;
      return FALSE;
   end HasDownRequest;

   procedure controller is
      floorRequestAbove : constant Boolean := HasFloorRequest(floor_buttons_array, floor + 1, TOP_FLOOR);
      floorRequestBelow : constant Boolean := HasFloorRequest(floor_buttons_array, 0, floor - 1);
      upRequestAbove : constant Boolean := HasUpRequest(up_buttons_array, floor + 1, TOP_FLOOR);
      upRequestBelow : constant Boolean := HasUpRequest(up_buttons_array, 0, floor - 1);
      downRequestAbove : constant Boolean := HasDownRequest(down_buttons_array, floor + 1, TOP_FLOOR);
      downRequestBelow : constant Boolean := HasDownRequest(down_buttons_array, 0, floor - 1);
   begin
      if motor = STOPPED then
         case door is
            when CLOSED =>
               if direction = UP then -- direction = Up
                  if ((floor_buttons_array(floor) = TRUE) or
                        (floor /= TOP_FLOOR and then up_buttons_array(floor) = TRUE))
                  then
                     DoorClosed2Half_Up;
                     Put_Line("=Controller= Door opens from Closed to Half (while going up)");
                     door_closing := False;
                  else
                     if (floorRequestAbove or upRequestAbove or downRequestAbove) then
                        MotorWinds;
                        Put_Line("=Controller= Motor starts winding");
                     else
                        if floor /= 0 and then down_buttons_array(floor) = TRUE then
                           ChangesDirectionDown_CurrentFloor;
                           Put_Line("=Controller= Changes direction to DOWN");
                        elsif (floorRequestBelow or upRequestBelow or downRequestBelow) then
                           ChangesDirectionDown_DownFloor;
                           Put_Line("=Controller= Changes direction to DOWN");
                        end if;
                     end if;
                  end if;
               else -- direction = Down
                  if ((floor_buttons_array(floor) = TRUE) or
                        (floor /= 0 and then down_buttons_array(floor) = TRUE))
                  then
                     DoorClosed2Half_Down;
                     Put_Line("=Controller= Door opens from Closed to Half (while going down)");
                     door_closing := False;
                  else
                     if (floorRequestBelow or upRequestBelow or downRequestBelow) then
                        MotorUnwinds;
                        Put_Line("=Controller= Motor starts unwinding");
                     else
                        if floor /= TOP_FLOOR and then up_buttons_array(floor) = TRUE then
                           ChangesDirectionUp_CurrentFloor;
                           Put_Line("=Controller= Changes direction to UP");
                        elsif (floorRequestAbove or upRequestAbove or downRequestAbove) then
                           ChangesDirectionUp_UpFloor;
                           Put_Line("=Controller= Changes direction to UP");
                        end if;
                     end if;
                  end if;
               end if;
            when OPEN =>
               DoorOpen2Half;
               Put_Line("=Controller= Door closes from Open to Half");
               door_closing := True;
            when HALF =>
               if (door_closing) then
                  if (direction = Up) then
                     if (floor_buttons_array(floor) = FALSE and then
                           (if floor /= TOP_FLOOR then up_buttons_array(floor) = FALSE))
                     then
                        DoorHalf2Closed;
                        Put_Line("=Controller= Door closes from Half to Closed");
                     else
                        DoorHalf2Open_Up;
                        Put_Line("=Controller= Door opens from Half to Open (while going up)");
                     end if;
                  else
                     if (floor_buttons_array(floor) = FALSE and then
                           (if floor /= 0 then down_buttons_array(floor) = FALSE))
                     then
                        DoorHalf2Closed;
                        Put_Line("=Controller= Door closes from Half to Closed");
                     else
                        DoorHalf2Open_Down;
                        Put_Line("=Controller= Door opens from Half to Open (while going down)");
                     end if;
                  end if;
               else
                  if (direction = Up) then
                     DoorHalf2Open_Up;
                     Put_Line("=Controller= Door opens from Half to Open (while going up)");
                  else
                     DoorHalf2Open_Down;
                     Put_Line("=Controller= Door opens from Half to Open (while going down)");
                  end if;
               end if;
         end case;
      else -- motor /= STOPPED
         if direction = UP then
            if (floor_buttons_array(floor) = TRUE) or
              (floor /= TOP_FLOOR and then up_buttons_array(floor) = TRUE)
            then
               MotorStopsServeRequest;
               Put_Line("=Controller= Motor stops to serve requests");
            else
               if not floorRequestAbove and not upRequestAbove and not downRequestAbove then
                  MotorStopsNoRequest;
                  Put_Line("=Controller= Motor stops (no more requests)");
               end if;
            end if;
         else -- direction = DOWN
            if (floor_buttons_array(floor) = TRUE) or
              (floor /= 0 and then down_buttons_array(floor) = TRUE)
            then
               MotorStopsServeRequest;
               Put_Line("=Controller= Motor stops to serve requests");
            else
               if not floorRequestBelow and not upRequestBelow and not downRequestBelow then
                  MotorStopsNoRequest;
                  Put_Line("=Controller= Motor stops (no more requests)");
               end if;
            end if;
         end if;
      end if;
   end controller;


end Controller_Package;

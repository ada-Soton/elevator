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
package body Elevator with SPARK_Mode is

   -- BEGIN Translation of Event-B events
   procedure INITIALISATION is
   begin
      floor := 0;
      direction := UP;
      motor := STOPPED;
      door := OPEN;
      floor_buttons_array := (others => False);
      up_buttons_array := (others => False);
      down_buttons_array := (others => False);
   end INITIALISATION;

   procedure MovesUp is
   begin
      floor := floor + 1;
   end MovesUp;

   procedure MovesDown is
   begin
      floor := floor - 1;
   end MovesDown;

   procedure ChangesDirectionUp_CurrentFloor is
   begin
      direction := Up;
   end ChangesDirectionUp_CurrentFloor;

   procedure ChangesDirectionUp_UpFloor is
   begin
      direction := Up;
   end ChangesDirectionUp_UpFloor;

   procedure ChangesDirectionDown_CurrentFloor is
   begin
      direction := Down;
   end ChangesDirectionDown_CurrentFloor;

   procedure ChangesDirectionDown_DownFloor is
   begin
      direction := Down;
   end ChangesDirectionDown_DownFloor;

   procedure MotorWinds is
   begin
      motor := WINDING;
   end MotorWinds;

   procedure MotorUnwinds is
   begin
      motor := UNWINDING;
   end MotorUnwinds;

   procedure MotorStopsServeRequest is
   begin
      motor := STOPPED;
   end MotorStopsServeRequest;

   procedure MotorStopsNoRequest is
   begin
      motor := STOPPED;
   end MotorStopsNoRequest;

   procedure DoorOpen2Half is
   begin
      door := HALF;
   end DoorOpen2Half;

   procedure DoorHalf2Closed is
   begin
      door := CLOSED;
   end DoorHalf2Closed;

   procedure DoorClosed2Half_Up is
   begin
      door := Half;
      floor_buttons_array(floor) := FALSE;
      if floor /= TOP_FLOOR then
         up_buttons_array(floor) := FALSE;
      end if;
   end DoorClosed2Half_Up;

   procedure DoorClosed2Half_Down is
   begin
      door := Half;
      floor_buttons_array(floor) := FALSE;
      if floor /= 0 then
         down_buttons_array(floor) := FALSE;
      end if;
   end DoorClosed2Half_Down;

   procedure DoorHalf2Open_Up is
   begin
      door := Open;
      floor_buttons_array(floor) := FALSE;
      if floor /= TOP_FLOOR then
         up_buttons_array(floor) := FALSE;
      end if;
   end DoorHalf2Open_Up;

   procedure DoorHalf2Open_Down is
   begin
      door := Open;
      floor_buttons_array(floor) := FALSE;
      if floor /= 0 then
         down_buttons_array(floor) := FALSE;
      end if;
   end DoorHalf2Open_Down;

   procedure FloorButtonPresses(f : Integer) is
   begin
      floor_buttons_array(f) := TRUE;
   end FloorButtonPresses;

   procedure UpButtonPresses(f : Integer) is
   begin
      up_buttons_array(f) := TRUE;
   end UpButtonPresses;

   procedure DownButtonPresses(f : Integer) is
   begin
      down_buttons_array(f) := TRUE;
   end DownButtonPresses;

   -- END Translation of Event-B events

   procedure Put is
   begin
      Put_Line("** System status **");
      Put("Floor -> ");Put(Integer'Image(floor)); Put(", ");
      Put("Motor -> "); Put(MOTOR_Type'Image(motor)); Put(", ");
      Put("Direction -> "); Put(DIRECTION_Type'Image(direction)); Put(", ");
      Put("Door -> "); Put(DOOR_Type'Image(door)); Put_Line("");
      Put("Enabled floor buttons -> ");
      for i in floor_buttons_array'Range loop
         if floor_buttons_array(i) then
            Put(i); Put(",");
         end if;
      end loop;
      Put_Line("");
      Put("Enabled up buttons -> ");
      for i in up_buttons_array'Range loop
         if up_buttons_array(i) then
            Put(i); Put(",");
         end if;
      end loop;
      Put_Line("");
      Put("Enabled down buttons -> ");
      for i in down_buttons_array'Range loop
         if down_buttons_array(i) then
            Put(i); Put(",");
         end if;
      end loop;
      Put_Line("");
      Put_Line("*******************");
   end Put;


end Elevator;

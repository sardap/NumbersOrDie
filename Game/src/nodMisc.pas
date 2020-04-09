unit nodMisc;

interface
	uses SwinGame, sgTypes, nodTypes, sgInputBackend, sgDriverInput, sgDriver, typinfo;

	//
	// Genral Functions
	//

	//
	//
	function CalcProportionWhole(proportion : Single; whole : Integer): Integer;
	//
	//
	function MillisecondsToSeconds(milliseconds: Integer): Integer;

	//
	// Side bar
	//

	//
	// gets the xside bar location
	function XSideBarLocation(): Integer;
	//
	// gets width of the side bar
	function CalcSideBarWidth(): Integer;
	//
	//
	procedure GetDirection(var xDirection, yDirection: Integer; xStart, widthStart, yStart, heightStart, xTarget, widthTarget, yTarget, heightTarget, speed: Integer);


	//
	// Speed Function
	//
	function UpdateSpeedChaos(const speed: Integer): Integer;

	//
	// MessageBox/Button Functions
	//
	function CheckButtonPressed(var toCheck: Button): Boolean;

	function CenterInsideBox(const box, length, size : Integer): Integer;

	//
	// Timer Function
	//
	procedure PauseAllTimers(var timerData: Times);

	procedure ResumeAllTimers(var timerData: Times);

	//
	// Random Placement
	//

	//
	//
	function XRandomPlacement(width: Integer): Integer; OverLoad;
	//
	// With Bitmap
	function XRandomPlacement(const avatar: Bitmap): Integer; OverLoad;
	//
	//
	function YRandomPlacement(height: Integer): Integer; OverLoad;
	//
	//
	function YRandomPlacement(const avatar: Bitmap): Integer; OverLoad;

implementation

	//
	// Genral Functions
	//

	//
	//
	function CalcProportionWhole(proportion : Single; whole : Integer): Integer;
	begin
		result := Round(proportion * whole);
	end;
	//
	//
	function MillisecondsToSeconds(milliseconds: Integer): Integer;
	begin
		result := milliseconds >> 10;
	end;
	//
	// gets width of the side bar
	function CalcSideBarWidth(): Integer;
	begin
		if ScreenWidth() < 1280 then
			result := CalcProportionWhole(0.15, ScreenWidth())
		else
			result := CalcProportionWhole(0.11, ScreenWidth());
	end;
	//
	// gets the xside bar location
	function XSideBarLocation(): Integer;
	begin
		result := ScreenWidth() - CalcSideBarWidth();
	end;
	//
	//
	procedure GetDirection(var xDirection, yDirection: Integer; xStart, widthStart, yStart, heightStart, xTarget, widthTarget, yTarget, heightTarget, speed: Integer);
	begin
		speed := abs(speed);
		// diagonal Movement
		if (xStart > xTarget) then
			xDirection := speed * -1
		else
			xDirection := speed;
		// diagonal Movement
		if (yStart > yTarget) then
			yDirection := speed * -1
		else
			yDirection := speed;
		//  Left Movement
		if (xStart > xTarget) and (yStart < yTarget) and (heightStart > yTarget) then
		begin
			xDirection := speed * -1;
			yDirection := 0;
		end;
		//  Right Movement
		if (xStart < xTarget) and (yStart < yTarget) and (heightStart > yTarget) then
		begin
			xDirection := speed;
			yDirection := 0;
		end
		//  Down Movement
		else if (xStart < xTarget) and (yStart < yTarget) and (widthStart > heightTarget)  then
		begin
			xDirection := 0;
			yDirection := speed;
		end;
		//  up Movement
		if (xStart < xTarget) and (yStart > yTarget) and (widthStart > widthTarget) then
		begin
			xDirection := 0;
			yDirection := speed * -1;
		end
	end;

	//
	// Speed Function
	//
	function UpdateSpeedChaos(const speed: Integer): Integer;
	begin
		result := speed * CHOAS_INCREMENT;
	end;

	function CheckButtonPressed(var toCheck: Button): Boolean;
	begin
		result := false;
		if MouseClicked(LeftButton) then
			if (MouseX() >= toCheck.postion.x) and (MouseX() <= toCheck.postion.width + toCheck.postion.x) and (MouseY() >= toCheck.postion.y) and (MouseY() <= toCheck.postion.height + toCheck.postion.y)  then
				result := true;

		if (toCheck.valueChk <> -1) then
			if (result) then
				toCheck.backgroundColor := ColorDarkRed
			else
				toCheck.backgroundColor := ColorDarkgray;
	end;

	function CenterInsideBox(const box, length, size : Integer): Integer;
	begin
		// box = x || y, length = Width || height, size = size
		result := box + (length >> 1) - (size >> 1);
	end;

	//
	// Timer Function
	//

	procedure PauseAllTimers(var timerData: Times);
	begin
		PauseTimer(timerData.worldTimer);
		PauseTimer(timerData.difficultyTimer);
		PauseTimer(timerData.amountHistoryTimer);
	end;

	procedure ResumeAllTimers(var timerData: Times);
	begin
		ResumeTimer(timerData.worldTimer);
		ResumeTimer(timerData.difficultyTimer);
		ResumeTimer(timerData.amountHistoryTimer);
	end;

	//
	// Random Placement
	//

	//
	//
	function XRandomPlacement(width: Integer): Integer; OverLoad;
	begin
		result := Rnd(XSideBarLocation() - width);
		if result < CalcSideBarWidth() then
			result += CalcSideBarWidth();
	end;
	//
	// With Bitmap
	function XRandomPlacement(const avatar: Bitmap): Integer; OverLoad;
	begin
		result := XRandomPlacement(BitmapWidth(avatar));
	end;
	//
	//
	function YRandomPlacement(height: Integer): Integer; OverLoad;
	begin
		result := Rnd(ScreenHeight() - height);
	end;
	//
	//
	function YRandomPlacement(const avatar: Bitmap): Integer; OverLoad;
	begin
		result := YRandomPlacement(BitmapHeight(avatar));
	end;

end.

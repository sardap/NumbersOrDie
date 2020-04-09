unit nodDraw;

interface
	uses SwinGame, sgTypes, nodTypes, nodMisc, nodCurrency, math;

	procedure DrawGame(const data: Game);

	procedure DrawMessageBox(var messageData: MessageBox);
	//
	//  Postion Functions/procedure
	//

	//
	//
	procedure UpdateCurrencyValuesPostionArray(var currencyValuePostion: BitmapTextArray; const buyValue: SingleArray; const selectedKind: CurrencyKind; const height: Integer);
	//
	//
	procedure UpdateAllValuesPostionsArray(var UserInterfacePostionData : UserInterfacePostion; const currencyValue: MarketValueArray);

	procedure InitialiseUserInterfacePostion(var UserInterfacePostionData: UserInterfacePostion);

implementation
	//
	//
	procedure DrawEntity(const avatar: Bitmap; const x: Integer; const y: Integer); OverLoad;
	begin
		DrawBitmap(avatar, x, y);
	end;
	//
	// Draws an enemy
	procedure DrawEntity(const data: Enemy); OverLoad;
	begin
		DrawEntity(data.avatar, data.x, data.y);
	end;
	//
	// Draws a Player
	procedure DrawEntity(const data: Player); OverLoad;
	begin
		DrawEntity(data.avatar, data.x, data.y);
	end;
	//
	// Draws a Projectile
	procedure DrawEntity(const data: Projectile); OverLoad;
	begin
		DrawEntity(data.avatar, data.x, data.y);
	end;

	//
	// Draw Array
	//

	//
	// Draws an enemy array
	procedure DrawArray(const data: EnemyArray); OverLoad;
	var
		i : Integer;

	begin
		for i:=0 to High(data) do
			DrawEntity(data[i]);
	end;
	//
	// Draws an ProjectileArray
	procedure DrawArray(const data: ProjectileArray); OverLoad;
	var
		i: Integer;

	begin
		for i:=0 to High(data) do
			DrawEntity(data[i]);
	end;

	//
	// Draw World Functions/procedure
	//

	//
	// Draws the World Backround
	procedure DrawBackground(const backgroundColor: Color);
	begin
		ClearScreen(backgroundColor);
	end;

	//
	// Draw UserInterface Functions/procedure
	//

	//
	// Draws a bargrpah
	procedure DrawBarGraph(const graph: ItemPostion; const amountHistory: SingleArray); //Bar chart for now
	var
		i, barX, barY, barHeight, middleHeight, barWidth, mutipler: Integer;
		clr : Color;

	begin
		barWidth := Ceil(graph.width / Length(amountHistory));
		mutipler := 1;
		middleHeight := graph.height >> 1;
		// sacales the bar so it fits
		for i:=0 to High(amountHistory) do
			//Check to make sure it is not zero so it doesn't waste resocues
			if (amountHistory[i] <> 0) then
				if (Round(Abs(amountHistory[i]) / mutipler) > middleHeight) then
						mutipler += 1;

		for i:=0 to High(amountHistory) do
		begin
			if amountHistory[i] = amountHistory[i+1] then
				clr := ColorGrey
			else if amountHistory[i] > amountHistory[i+1] then
				clr := ColorGreen
			else
				clr := ColorRed;
			barHeight := Round(amountHistory[i] / mutipler);
			barY := (middleHeight - barHeight) + graph.y;
			barX := graph.x + (barWidth * i);
			{
			becuase of the size of the screen is unknown i ether had to round down which would have it be far to short or round up
			wich is far to large so im rounding up but only if it runs
			past it it will not show
			}
			if (barX < (graph.x + graph.width)) then
				FillRectangle(clr, barX, barY, barWidth, barHeight);
		end;
	end;
	//
	// Draws a graph
	procedure DrawGraph(const graph: ItemPostion; const amountHistory: SingleArray);
	begin
		DrawRectangle(ColorBlack, graph.x, graph.y, graph.width, graph.height);
		DrawBarGraph(graph, amountHistory);
	end;
	//
	// Purpose: Draws currency Text with a $ and to 4 decimal points
	//
	procedure DrawCurrencyValue(const amount: Single; const currencyValueTextData : TextData);
	var
		conversion : String;

	begin
		Str(amount:0:4, conversion);
		DrawText(conversion, currencyValueTextData.clr, currencyValueTextData.fontType, currencyValueTextData.size, currencyValueTextData.x, currencyValueTextData.y);
	end;
	//
	// Draws the buy vaules in order expect for the selected kind
	//
	procedure DrawBuyVaules(const currencyValueTitle, currencyValuePostion: BitmapTextArray);
	var
		i : Integer;

	begin
		for i:=0 to High(currencyValueTitle) do
		begin
			DrawBitmap(currencyValueTitle[i].avatar, currencyValueTitle[i].x, currencyValueTitle[i].y);
			DrawBitmap(currencyValuePostion[i].avatar, currencyValuePostion[i].x, currencyValuePostion[i].y);
		end;
	end;
	//
	// Purpose: Draws the Currecny Title (Pound, etc) and changes color depnding if the player has slected the currency
	//
	procedure DrawCurrencyTitle(const currencyTitle: TextData; const transfer: CurrencyPtrArray);
	var
		clr : Color;

	begin
		clr := currencyTitle.clr;
		if (transfer[0] <> nil) and (CurrencyKindToString(transfer[0]^.kind) = currencyTitle.toPrint) then
			clr := ColorBlue
		else if (transfer[1] <> nil) and (CurrencyKindToString(transfer[1]^.kind) = currencyTitle.toPrint) then
			clr := ColorOrange;
		DrawText(currencyTitle.toPrint, clr, currencyTitle.fontType, currencyTitle.size, currencyTitle.x, currencyTitle.y);
	end;
	//
	// Purpose: Draws a Single CurrecnyBox
	//
	procedure DrawCurrencyBox(const currencyBoxData: CurrencyBox; const currencyData: Currency; const currencyValue: MarketValue; const transfer: CurrencyPtrArray);
	begin
		// Draws the Border
		DrawRectangle(ColorBlack, currencyBoxData.postion.x, currencyBoxData.postion.y, currencyBoxData.postion.width, currencyBoxData.postion.height);
		// Draws the Currency name
		DrawCurrencyTitle(currencyBoxData.currencyTitle, transfer);
		// Draws the buy Values
		DrawBuyVaules(currencyBoxData.currencyValueTitle, currencyBoxData.currencyValuePostion);
		DrawGraph(currencyBoxData.graph, currencyData.amountHistory);
		// Draws the amount of currency Given to the Function
		DrawBitmap(currencyBoxData.currencyAmountTitle.avatar, currencyBoxData.currencyAmountTitle.x, currencyBoxData.currencyAmountTitle.y);
		DrawCurrencyValue(currencyData.amount, currencyBoxData.currencyAmountPostion);
	end;
	//
	// Purpose: Draws all 4 of the Currency Boxes
	//
	procedure DrawAllCurrencyBox(const UserInterfacePostionData : UserInterfacePostion; const  playerData	: Player; const currencyValue: MarketValueArray);
	begin
		//
		// Right Pannel
		FillRectangle(ColorWhite, UserInterfacePostionData.rightBarData.postion.x, UserInterfacePostionData.rightBarData.postion.y, UserInterfacePostionData.rightBarData.postion.width, UserInterfacePostionData.rightBarData.postion.height);
		// Top Right
		DrawCurrencyBox(UserInterfacePostionData.rightBarData.currencyBoxTop, playerData.currencyOwned[Integer(UserInterfacePostionData.rightBarData.currencyBoxTop.kind)], currencyValue[Integer(UserInterfacePostionData.rightBarData.currencyBoxTop.kind)], playerData.selectionData);
		// Bottom Right
		DrawCurrencyBox(UserInterfacePostionData.rightBarData.currencyBoxBot, playerData.currencyOwned[Integer(UserInterfacePostionData.rightBarData.currencyBoxBot.kind)], currencyValue[Integer(UserInterfacePostionData.rightBarData.currencyBoxBot.kind)], playerData.selectionData);
		//
		// Left Pannel
		FillRectangle(ColorWhite, UserInterfacePostionData.leftBarData.postion.x, UserInterfacePostionData.leftBarData.postion.y, UserInterfacePostionData.rightBarData.postion.width, UserInterfacePostionData.rightBarData.postion.height); //I hate pannels
		// Top Left
		DrawCurrencyBox(UserInterfacePostionData.leftBarData.currencyBoxTop, playerData.currencyOwned[Integer(UserInterfacePostionData.leftBarData.currencyBoxTop.kind)], currencyValue[Integer(UserInterfacePostionData.leftBarData.currencyBoxTop.kind)], playerData.selectionData);
		// Bottom Left
		DrawCurrencyBox(UserInterfacePostionData.leftBarData.currencyBoxBot, playerData.currencyOwned[Integer(UserInterfacePostionData.leftBarData.currencyBoxBot.kind)], currencyValue[Integer(UserInterfacePostionData.leftBarData.currencyBoxBot.kind)], playerData.selectionData);
	end;
	//
	// Purpose: Draws the World Timer at the given location
	//
	procedure DrawWorldTimer(const timerTitle : BitmapText; const worldTimer: TextData; const time: Timer; const worldState: WorldKind);
	var
		toPrintTimer : String;
		timeLeft : Integer;

	begin
		if worldState = Passive then
			timeLeft := TIME_PASSIVE_STATE - TimerTicks(time)
		else
			timeLeft := TIME_CHAOS_STATE - TimerTicks(time);
		// Draws the Title for the timer
		DrawBitmap(timerTitle.avatar, timerTitle.x, timerTitle.y);
		timeLeft := MillisecondsToSeconds(timeLeft);
		Str(timeLeft, toPrintTimer);
		DrawText(toPrintTimer, ColorBlack, worldTimer.fontType, worldTimer.size, worldTimer.x, worldTimer.y);
	end;
	//
	// Purpose: Draws the Score at the passed location
	//
	procedure DrawScore(scorePostion : TextData; const worldState: WorldKind; const netWorth: Single; const standard: CurrencyKind);
	var
		toPrint : String;

	begin
		if worldState = Passive then
			scorePostion.clr := ColorRed
		else
			scorePostion.clr := ColorGreen;
		toPrint := CurrencyKindToString(standard) + ':Networth:';
		DrawText(toPrint, scorePostion.clr, scorePostion.fontType, scorePostion.size, scorePostion.x, scorePostion.y);
		scorePostion.x += TextWidth(LoadFont(scorePostion.fontType, scorePostion.size), toPrint);
		DrawCurrencyValue(netWorth, scorePostion);
	end;
	//
	// Purpose: Draws the UserInterface
	//
	procedure DrawUserInterface(const gameData: Game);
	begin
		DrawAllCurrencyBox(gameData.UserInterfacePostionData, gameData.playerData, gameData.marketData.currencyValue);
		DrawWorldTimer(gameData.UserInterfacePostionData.rightBarData.worldTimerTitle, gameData.UserInterfacePostionData.rightBarData.worldTimer, gameData.timeData.worldTimer, gameData.worldData.worldState);
		DrawScore(gameData.UserInterfacePostionData.scorePostion, gameData.worldData.worldState, gameData.playerData.netWorth, gameData.marketData.standard);
	end;
	//
	// Purpose: Draws entire Game
	//
	procedure DrawGame(const data: Game);
	begin
		DrawBackground(data.worldData.backgroundColor);
		DrawArray(data.enemyData);
		DrawEntity(data.playerData);
		DrawArray(data.projectileData);
		DrawUserInterface(data);
	end;
	//
	// Purpose: Draws a Message Box
	//
	procedure DrawMessageBox(var messageData: MessageBox);
	var
		i : Integer;

	begin
		DrawFramerate(messageData.whiteBox.x, messageData.whiteBox.y);

		FillRectangle(ColorWhite, messageData.whiteBox.x, messageData.whiteBox.y, messageData.whiteBox.width, messageData.whiteBox.height);

		DrawRectangle(ColorBlack, messageData.whiteBox.x, messageData.whiteBox.y, messageData.whiteBox.width, messageData.whiteBox.height);
		// Draws the Title
		DrawBitmap(messageData.title.avatar, messageData.title.x, messageData.title.y);
		// Draws Res Button's and text
		for i:=0 to High(messageData.buttonData) do
		begin
			// Draws Button
			FillRectangle(messageData.buttonData[i].backgroundColor, messageData.buttonData[i].postion.x, messageData.buttonData[i].postion.y, messageData.buttonData[i].postion.width, messageData.buttonData[i].postion.height);
			// Draws Text
			DrawBitmap(messageData.buttonData[i].text.avatar, messageData.buttonData[i].text.x, messageData.buttonData[i].text.y);
		end;

	end;
	//
	// Purpose: Updates the values of a single the currencyValue Text
	//
	procedure UpdateCurrencyValuesPostionArray(var currencyValuePostion: BitmapTextArray; const buyValue: SingleArray; const selectedKind: CurrencyKind; const height: Integer);
	var
		i, j: Integer;
		toPrint, conversion : String;

	begin
		j:= 0;
		for i:=0 to Integer(CurrencyKindLength)-1 do
			if not(i = Integer(selectedKind)) then
			begin
				Str(buyValue[i]:0:3, conversion);
				toPrint := '$' + conversion;
				currencyValuePostion[j].avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.049, height)), toPrint, ColorBlack, ColorWhite);
				j += 1;
			end;
	end;
	//
	// Purpose: Updates all four Values for each Currecny
	//
	procedure UpdateAllValuesPostionsArray(var UserInterfacePostionData : UserInterfacePostion; const currencyValue: MarketValueArray);
	begin
		UpdateCurrencyValuesPostionArray(UserInterfacePostionData.rightBarData.currencyBoxTop.currencyValuePostion, currencyValue[Integer(UserInterfacePostionData.rightBarData.currencyBoxTop.kind)].buyValue, UserInterfacePostionData.rightBarData.currencyBoxTop.kind, UserInterfacePostionData.rightBarData.currencyBoxTop.postion.height);

		UpdateCurrencyValuesPostionArray(UserInterfacePostionData.rightBarData.currencyBoxBot.currencyValuePostion, currencyValue[Integer(UserInterfacePostionData.rightBarData.currencyBoxBot.kind)].buyValue, UserInterfacePostionData.rightBarData.currencyBoxBot.kind, UserInterfacePostionData.rightBarData.currencyBoxBot.postion.height);

		UpdateCurrencyValuesPostionArray(UserInterfacePostionData.leftBarData.currencyBoxTop.currencyValuePostion, currencyValue[Integer(UserInterfacePostionData.leftBarData.currencyBoxTop.kind)].buyValue, UserInterfacePostionData.leftBarData.currencyBoxTop.kind, UserInterfacePostionData.leftBarData.currencyBoxTop.postion.height);

		UpdateCurrencyValuesPostionArray(UserInterfacePostionData.leftBarData.currencyBoxBot.currencyValuePostion, currencyValue[Integer(UserInterfacePostionData.leftBarData.currencyBoxBot.kind)].buyValue, UserInterfacePostionData.leftBarData.currencyBoxBot.kind, UserInterfacePostionData.leftBarData.currencyBoxBot.postion.height);

	end;
	//
	// Purpose: Updates all four Values for each Currecny
	//
	procedure InitialiseTitlePostion(var currencyTitle: TextData; const box: ItemPostion; const selectedKind: CurrencyKind);
	begin
		currencyTitle.x := box.x + CalcProportionWhole(0.38, box.width);
		currencyTitle.y := box.y + CalcProportionWhole(0.10, box.Height);
		currencyTitle.size := CalcProportionWhole(0.090, box.width);
		currencyTitle.clr := ColorBlack;
		currencyTitle.fontType := 'Calibri';
		currencyTitle.toPrint := CurrencyKindToString(selectedKind);
	end;
	//
	// Purpose: Set's the reletive Location for the titles for the Currecnys
	//
	procedure InitialiseAmountTitlePostion(var currencyAmountTitle: BitmapText; const box: ItemPostion);
	begin
		currencyAmountTitle.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.050, box.height)), 'Ammount:', ColorBlack, ColorWhite);
		currencyAmountTitle.x := box.x + 2;
		currencyAmountTitle.y := box.y + CalcProportionWhole(0.20, box.height);
	end;
	//
	// Purpose: Set's the reletive for each of the ammount of Currecnys
	//
	procedure InitialiseAmountPostion(var currencyAmountPostion: TextData; const currencyAmountTitle: BitmapText; height: Integer);
	begin
		currencyAmountPostion.x := currencyAmountTitle.x + BitmapWidth(currencyAmountTitle.avatar);
		currencyAmountPostion.y := currencyAmountTitle.y;
		currencyAmountPostion.size := CalcProportionWhole(0.050, height);
		currencyAmountPostion.clr := ColorBlack;
		currencyAmountPostion.fontType := 'Calibri';
	end;

	procedure InitialiseGraphPostion(var graph: ItemPostion; const box: ItemPostion);
	begin
		graph.x := box.x;
		graph.y := box.y + CalcProportionWhole(GRAPH_HEIGHT_PROPORTION, box.height);
		graph.width := box.width;
		graph.height := CalcProportionWhole(GRAPH_HEIGHT_PROPORTION, box.height);
	end;
	//
	// Purpose: Set's the reletive Postion for each of the Buy Values Buy: text
	//
	procedure InitialiseValueTitleArrayPostion(var currencyValueTitle: BitmapTextArray; x, y, height : Integer; const selectedKind: CurrencyKind);
	var
		i, j : Integer;
		toPrint : String;

	begin
		// sets the size of the array to the number of CurrencyKinds -1
		SetLength(currencyValueTitle, Integer(CurrencyKindLength)-1);
		j := 0;
		for i:=0 to Integer(CurrencyKindLength)-1 do
		begin
			// Skips the Selcted Kind
			if not(i = Integer(selectedKind)) then
			begin
				toPrint := CurrencyKindToString(CurrencyKind(i)) + ' Buy:';
				currencyValueTitle[j].avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.049, height)), toPrint, ColorBlack, ColorWhite);
				currencyValueTitle[j].x := x + 2;
				currencyValueTitle[j].y := y;
				y += BitmapHeight(currencyValueTitle[j].avatar) + 1;
				j += 1;
			end;
		end;
	end;
	//
	// Purpose: Set's the reletive Postion for the value of the Currency
	//
	procedure InitialiseValuesArrayPostion(var currencyValuePostion : BitmapTextArray; const currencyValueTitle: BitmapTextArray);
	var
		i: Integer;

	begin
		SetLength(currencyValuePostion, Length(currencyValueTitle));
		for i:=0 to High(currencyValuePostion) do
		begin
			currencyValuePostion[i].x := currencyValueTitle[i].x + BitmapWidth(currencyValueTitle[i].avatar);
			currencyValuePostion[i].y := currencyValueTitle[i].y;
		end;
	end;
	//
	// Purpose: Set's the reletive Postion for a Single CurrencyBox
	//
	procedure InitialiseCurrencyBox(var box: CurrencyBox; x, y, width, height: Integer; selectedKind: CurrencyKind);
	begin
		box.kind := selectedKind;

		box.postion.x := x;
		box.postion.y := y;
		box.postion.width := width;
		box.postion.height := height;

		InitialiseTitlePostion(box.currencyTitle, box.postion, box.kind);

		InitialiseAmountTitlePostion(box.currencyAmountTitle, box.postion);

		InitialiseAmountPostion(box.currencyAmountPostion, box.currencyAmountTitle, box.postion.height);

		InitialiseGraphPostion(box.graph, box.postion);

		InitialiseValueTitleArrayPostion(box.currencyValueTitle, box.postion.x, box.graph.y + box.graph.height, box.postion.height, box.kind);

		InitialiseValuesArrayPostion(box.currencyValuePostion, box.currencyValueTitle);

	end;
	//
	// Purpose: Set's the reletive Postion for the Score
	//
	procedure InitialiseScorePostion(var scorePostion : TextData; const box: ItemPostion);
	begin
		scorePostion.size := CalcProportionWhole(0.020, ScreenWidth);
		scorePostion.x := box.x + box.width + scorePostion.size div 3;
		scorePostion.y := box.y + scorePostion.size;
		scorePostion.clr := ColorBlack;
		scorePostion.fontType := 'Calibri';
	end;

	procedure InitialiseTimerTitlePostion(var worldTimerTitle: BitmapText; const box: ItemPostion);
	begin
		worldTimerTitle.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.090, box.width)), 'Time Left', ColorBlack, ColorWhite);
		worldTimerTitle.x := CenterInsideBox(box.x, box.width, BitmapWidth(worldTimerTitle.avatar));
		worldTimerTitle.y := box.y + CalcProportionWhole(0.87, box.height);
	end;

	procedure InitialiseTimerPostion(var worldTimer: TextData; const worldTimerTitle: BitmapText);
	begin
		//puts the location to be directly bellow the Title of the Timer
		worldTimer.size := CalcProportionWhole(0.70, BitmapWidth(worldTimerTitle.avatar));
		worldTimer.fontType := 'Calibri';
		worldTimer.x := CenterInsideBox(worldTimerTitle.x, BitmapWidth(worldTimerTitle.avatar), TextWidth(LoadFont(worldTimer.fontType, worldTimer.size), '000'));
		worldTimer.y := worldTimerTitle.y + BitmapHeight(worldTimerTitle.avatar);
		worldTimer.clr := ColorBlack;

	end;

	procedure InitialiseUserInterfacePostion(var UserInterfacePostionData: UserInterfacePostion);
	var
		currencyBoxHeight, sideBarWidth: Integer;

	begin
		currencyBoxHeight := CalcProportionWhole(0.40, ScreenHeight());
		sideBarWidth := CalcSideBarWidth();

		//
		//Right Bar postion
		UserInterfacePostionData.rightBarData.postion.x := 0;
		UserInterfacePostionData.rightBarData.postion.y := 0;
		UserInterfacePostionData.rightBarData.postion.width := sideBarWidth;
		UserInterfacePostionData.rightBarData.postion.height := ScreenHeight();

		//
		// Sidebar Right Top Pound
		InitialiseCurrencyBox(UserInterfacePostionData.rightBarData.currencyBoxTop, UserInterfacePostionData.rightBarData.postion.x, UserInterfacePostionData.rightBarData.postion.y, UserInterfacePostionData.rightBarData.postion.width, currencyBoxHeight, Pound);

		//
		// Sidebar Right Bot Denarius
		InitialiseCurrencyBox(UserInterfacePostionData.rightBarData.currencyBoxBot, UserInterfacePostionData.rightBarData.postion.x, UserInterfacePostionData.rightBarData.postion.y + currencyBoxHeight, UserInterfacePostionData.rightBarData.postion.width, currencyBoxHeight, Denarius);

		// World timer
		InitialiseTimerTitlePostion(UserInterfacePostionData.rightBarData.worldTimerTitle, UserInterfacePostionData.rightBarData.postion);

		InitialiseTimerPostion(UserInterfacePostionData.rightBarData.worldTimer, UserInterfacePostionData.rightBarData.worldTimerTitle);


		//
		//Left Bar postion
		UserInterfacePostionData.leftBarData.postion.x := ScreenWidth() - sideBarWidth;
		UserInterfacePostionData.leftBarData.postion.y := 0;
		UserInterfacePostionData.leftBarData.postion.width := sideBarWidth;
		UserInterfacePostionData.leftBarData.postion.height := ScreenHeight();

		//
		// Sidebar Left Top DollaryDoo
		InitialiseCurrencyBox(UserInterfacePostionData.leftBarData.currencyBoxTop, UserInterfacePostionData.leftBarData.postion.x, UserInterfacePostionData.leftBarData.postion.y, UserInterfacePostionData.leftBarData.postion.width, currencyBoxHeight, DollaryDoo);

		//
		// Sidebar Left Bot Euro
		InitialiseCurrencyBox(UserInterfacePostionData.leftBarData.currencyBoxBot, UserInterfacePostionData.leftBarData.postion.x, UserInterfacePostionData.leftBarData.postion.y + currencyBoxHeight, UserInterfacePostionData.leftBarData.postion.width, currencyBoxHeight, Euro);

		//
		// Score
		InitialiseScorePostion(UserInterfacePostionData.scorePostion, UserInterfacePostionData.rightBarData.postion);

	end;
end.

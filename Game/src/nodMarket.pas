unit nodMarket;

interface
	uses SwinGame, sgTypes, nodTypes, nodMisc, nodCurrency, nodDraw;
	//
	// Market Functions/procedures
	//
	procedure UpdateMarket(var marketData: Market; var UserInterfacePostionData : UserInterfacePostion; const playerData: Player; const enemyData: EnemyArray; const worldState: WorldKind);

	procedure InitialiseMarket(var marketData: Market; var UserInterfacePostionData: UserInterfacePostion; const difficulty: Integer);

	function CalcCurrencyConversion(const amount: Single; const valueOfConversion: Single): Single;

	function CalculateNetWorth(const currencyData: CurrencyArray; const marketData: Market): Single;

implementation
	//
	// Market Functions/procedures
	//
	procedure UpdateBuyValue(var buyValue: SingleArray; const currentSum: Single; const currencyValue: MarketValueArray);
	var
		i : Integer;

	begin

		for i:= 0 to High(buyValue) do
			begin
				if (currencyValue[i].sum = 0) or (currentSum = 0) then
					buyValue[i] := 0
				else
					buyValue[i] := currencyValue[i].sum / currentSum;
				// WriteLn('Success');
			end
	end;
	//
	// Purpose: Updates Each of the buy Values
	//
	procedure UpdateMarketValueArray(var currencyValue: MarketValueArray; const playerData: Player; const enemyData: EnemyArray; const stability: Single);
	var
		i, j : Integer;

	begin
		// Sums everything
		for i:=0 to High(currencyValue) do
		begin
			currencyValue[i].sum := stability;
			currencyValue[i].sum += abs(playerData.currencyOwned[Integer(currencyValue[i].kind)].amount);
			for j:=0 to High(enemyData) do
				currencyValue[i].sum += abs(enemyData[j].currencyOwned[Integer(currencyValue[i].kind)].amount);
		end;
		//Upadtes the array
		for i:=0 to High(currencyValue) do
			UpdateBuyValue(currencyValue[i].buyValue, currencyValue[i].sum, currencyValue);
	end;
	//
	// Purpose: Returns the Stanard Currecny
	//
	function GetStandardCurrency(const currencyValue : MarketValueArray): CurrencyKind;
	var
		i, j : Integer;
		sum : SingleArray;
		max : Double;

	begin
		result := Pound;
		SetLength(sum, Length(currencyValue));
		//Sums Each Currency Bu
		for i:=0 to High(sum) do
		begin
			sum[i] := 0;
			for j:= 0 to High(currencyValue[i].buyValue) do
				sum[i] += currencyValue[i].buyValue[j];
		end;
		max := sum[0];
		for i:= 0 to High(currencyValue) do
		begin
			if (sum[i] > max) then
			begin
				max := sum[i];
				result := CurrencyKind(i);
			end;
		end;
	end;
	//
	// Purpose: Updates Each Value Inside of the market Record
	//
	procedure UpdateMarket(var marketData: Market; var UserInterfacePostionData : UserInterfacePostion; const playerData: Player; const enemyData: EnemyArray; const worldState: WorldKind);
	var
		stability : Single;

	begin
		if worldState = Passive then
			stability := marketData.stability
		else
			stability := 0;
		UpdateMarketValueArray(marketData.currencyValue, playerData, enemyData, stability);
		marketData.standard := GetStandardCurrency(marketData.currencyValue);
		UpdateAllValuesPostionsArray(UserInterfacePostionData, marketData.currencyValue);
	end;

	procedure InitialiseBuyValue(var buyValue : SingleArray);
	var
		i : Integer;

	begin
		SetLength(buyValue, Integer(CurrencyKindLength));
		for i:=0 to High(buyValue) do
			buyValue[i] := 1;
	end;

	procedure InitialiseCurrencyValue(var currencyValue: MarketValueArray);
	var
		i : Integer;

	begin
		SetLength(currencyValue, Integer(CurrencyKindLength));
		for i:=0 to High(currencyValue) do
		begin
			currencyValue[i].sum := 0;
			currencyValue[i].kind := CurrencyKind(i);
			InitialiseBuyValue(currencyValue[i].buyValue);
		end;
	end;

	procedure InitialiseMarket(var marketData: Market; var UserInterfacePostionData: UserInterfacePostion; const difficulty: Integer);
	begin
		InitialiseCurrencyValue(marketData.currencyValue);
		marketData.standard := GetStandardCurrency(marketData.currencyValue);
		marketData.stability := Round(10 / difficulty * 100000);
		UpdateAllValuesPostionsArray(UserInterfacePostionData, marketData.currencyValue);
	end;

	function CalcCurrencyConversion(const amount: Single; const valueOfConversion: Single): Single;
	begin
		result := amount * valueOfConversion;
	end;

	function CalculateNetWorth(const currencyData: CurrencyArray; const marketData: Market): Single;
	var
		i : Integer;

	begin
		result := 0;
		for i:= 0 to High(currencyData) do
			result += CalcCurrencyConversion(currencyData[i].amount, marketData.currencyValue[i].buyValue[Integer(marketData.standard)]);
	end;
end.

unit nodCurrency;

interface
    uses SwinGame, sgTypes, nodTypes, nodMisc;

    //
    // Currency functions/procedures
    //

    //
    // Returns a string based on a kind
    function CurrencyKindToString(kind: CurrencyKind): String;

    //
    // Ammount History
    //

    //
    // Moves everything in the array down one
    procedure UpdateAmountHistory(var amountHistory: SingleArray; const amount: Single);
    //
    //
    procedure InitialiseAmountHistory(var amountHistory: SingleArray; const amount: Single);

    //
    // Initialise Currency
    //

    //
    // Sets all values to the diffcilty
    procedure InitialiseCurrencyPlayer(var currencyOwned: CurrencyArray);
    //
    // Sets all values to a random number with a max of diffcilty
    procedure InitialiseCurrencyEnemy(var currencyOwned: CurrencyArray; const difficulty: Integer; const kind : EnemyKind);

    //
    // initialises the Currency Ptr Array
    //

    //
    //
    procedure InitialiseCurrencyPtrArray(var selectedCurrency: CurrencyPtrArray);
		//
		// Makes the wealth Evenly distbuted
		procedure RedistributeWealth(var playerWealth: CurrencyArray; var enemyData: EnemyArray);

implementation
//
    // Currency functions/procedures
    //

    //
    // Returns a string based on a kind
    function CurrencyKindToString(kind: CurrencyKind): String;
    begin
        result := '';
        case kind of
            Pound : result := 'Pound';
            DollaryDoo : result := 'DollaryDoo';
            Denarius : result := 'Denarius';
            Euro : result := 'Euro';
        end;
    end;

    //
    // Ammount History
    //

    //
    // Moves everything in the array down one
    procedure UpdateAmountHistory(var amountHistory: SingleArray; const amount: Single);
    var
        i : Integer;

    begin
        for i:= High(amountHistory) downto 1 do
            amountHistory[i] := amountHistory[i-1];
        amountHistory[0] := amount;
    end;

    //
    //
    procedure InitialiseAmountHistory(var amountHistory: SingleArray; const amount: Single);
    begin
        SetLength(amountHistory, 100);
    end;

    //
    // Initialise Currency
    //

    //
    // Sets all values to the diffcilty
    procedure InitialiseCurrencyPlayer(var currencyOwned: CurrencyArray);
    var
        i : Integer;

    begin
        SetLength(currencyOwned, Integer(CurrencyKindLength));
        for i:=0 to High(currencyOwned) do
        begin
            currencyOwned[i].amount := STARTING_PLAYER_MONEY;
            currencyOwned[i].kind := CurrencyKind(i);
            InitialiseAmountHistory(currencyOwned[i].amountHistory, currencyOwned[i].amount);
        end;
    end;
    //
    // Sets all values to a random number with a max of diffcilty
    procedure InitialiseCurrencyEnemy(var currencyOwned: CurrencyArray; const difficulty: Integer; const kind : EnemyKind);
    var
        i, wealthToDistribution : Integer;

    begin
        // genrated a Random number based on the diffiuclty then diveds it by the amount of currencys kinds
        wealthToDistribution := (Rnd(difficulty)+1) div Integer(CurrencyKindLength);
        SetLength(currencyOwned, Integer(CurrencyKindLength));
        for i:=0 to High(currencyOwned) do
        begin
            //Genrates random number then adds cents to it
						if kind = EnemyRDTW then
							currencyOwned[i].amount := 1
						else // all other non revolutionary enemys
            	currencyOwned[i].amount := Rnd(wealthToDistribution) + ((Rnd(100)+1) / 100);
            currencyOwned[i].kind := CurrencyKind(i);
        end;
    end;


    //
    // initialises the Currency Ptr Array
    //

    //
    // purpose: Initialises the currency pointer array with the value of Null
		//
    procedure InitialiseCurrencyPtrArray(var selectedCurrency: CurrencyPtrArray);
    var
        i: Integer;

    begin
        SetLength(selectedCurrency, 2);
        for i:=0 to High(selectedCurrency) do
        begin
            selectedCurrency[i] := nil;
        end;
    end;

		//
    // purpose: Evenly Distubures all of the money among everything with money
		//
		procedure RedistributeWealth(var playerWealth: CurrencyArray; var enemyData: EnemyArray);
		var
			i, j, count : Integer;
			sumOfWealth, EvenlyDistubted : Double;

		begin
			sumOfWealth := 0;
			count := 0;
			// Sums enemys Money
			for i:=0 to High(enemyData) do
			begin
				if enemyData[i].kind <> EnemyRDTW then
				begin
					for j:=0 to High(enemyData[i].currencyOwned) do
					begin
						sumOfWealth += enemyData[i].currencyOwned[j].amount;
						// WriteLn(sumOfWealth:0:2);
					end;
					count += 1;
				end;
			end;
			// Sums Player Money
			for i:=0 to High(playerWealth) do
				sumOfWealth += playerWealth[i].amount;
			// WriteLn(sumOfWealth:0:2);
			count += 1;
			//Removes a Random Number from the sum
			// WriteLn(count);
      //Disbutbes the wealth evenly
			EvenlyDistubted := sumOfWealth / count;
			// WriteLn(sumOfWealth:0:2, ':', EvenlyDistubted:0:2);
			EvenlyDistubted := EvenlyDistubted / 4;
			// WriteLn(sumOfWealth:0:2, ':', EvenlyDistubted:0:2);
			for i:=0 to High(enemyData) do
				if enemyData[i].kind <> EnemyRDTW then
					for j:=0 to High(enemyData[i].currencyOwned) do
						enemyData[i].currencyOwned[j].amount := EvenlyDistubted;

			for i:=0 to High(playerWealth) do
				playerWealth[i].amount := EvenlyDistubted;
		end;
end.

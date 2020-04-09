unit nodTypes;

interface
	uses sgTypes;

	const
		OLD_GRAPH_HEIGHT_PROPORTION = 0.18;
		GRAPH_HEIGHT_PROPORTION = 0.40;
		START_PLAYER_SPEED = 5;
		TIME_CHAOS_STATE: Integer = 147000;
		TIME_PASSIVE_STATE: Integer = 20000;
		DIFFICULTY_TIMER_LOOP: Integer = 10000;
		CHOAS_INCREMENT: Integer = 2;
		STARTING_PLAYER_MONEY: Integer = 100;
		DEFAULT_DIFFICULTY = 1000;
		SETTINGS_FILE_NAME = 'settings.nod';
		SEND_SCORE_BOARD_PREFIX = 'Score:';
		UPLOAD_PREFIX = 'Upload:';

	type
		//
		// GENRAL TYPES
		//
		SingleArray = array of Single;
		SinglePtr = ^Single;
		IntegerArray = array of Integer;
		StringArray = array of String;

		//
		// Currency Types
		//
		CurrencyKind = (Pound, DollaryDoo, Denarius, Euro, CurrencyKindLength);
		Currency = record
			amount : Single;
			kind : CurrencyKind;
			amountHistory : SingleArray;
		end;
		CurrencyArray = array of Currency;
		CurrencyPtr = ^Currency;
		CurrencyPtrArray = array of CurrencyPtr;

		//
		// Market Types
		//
		MarketValue = record
			sum : Double;
			buyValue : SingleArray;
			kind : CurrencyKind;
		end;
		MarketValueArray = array of MarketValue;
		Market =  record
			currencyValue : MarketValueArray;
			standard : CurrencyKind;
			stability : Single;
		end;

		//
		// projectile
		//
		ProjectileKind = (ProjectileEnemy, ProjectilePlayer);
		Projectile = record
			proKind : ProjectileKind;
			damageKind : CurrencyKind;
			owner : CurrencyPtr;
			avatar : Bitmap;
			damage : Single;
			x : Integer;
			y : Integer;
			xDirection : Integer;
			yDirection : Integer;
			speed : Integer;
		end;
		ProjectileArray = array of Projectile;

		//
		// PLAYER TYPES
		//
		Player = record
			entityName : String;
			currencyOwned : CurrencyArray;
			netWorth : Single;
			selectionData : CurrencyPtrArray;
			avatar : Bitmap;
			x : Integer;
			y : Integer;
			xPrevious : Integer;
			yPrevious : Integer;
			speed : Integer;
		end;

		//
		// ENEMEY TYPES
		//
		EnemyKind = (EnemyTree, EnemyImp, EnemyRDTW, EnemyKindLength);
		Enemy = record
			kind : EnemyKind;
			currencyOwned : CurrencyArray;
			netWorth : Single;
			avatar : Bitmap;
			speed : Integer;
			x : Integer;
			y : Integer;
			xTarget : Integer;
			yTarget : Integer;
			xDirection : Integer;
			yDirection : Integer;
		end;
		EnemyArray = array of Enemy;

		//
		// WORLD TYPES
		//
		WorldKind = (Passive, Chaos);
		World = record
			worldState : WorldKind;
			backgroundColor : Color;
		end;

		//
		// Time Types
		//
		Times = record
			difficultyTimer : Timer;
			worldTimer : Timer;
			amountHistoryTimer : Timer;
		end;

		//
		// Item Postion
		//
		ItemPostion = record
			x : Integer;
			y : Integer;
			width : Integer;
			height : Integer;
		end;
		ItemPostionArray = array of ItemPostion;

		BitmapText = record
			x : Integer;
			y : Integer;
			avatar : Bitmap;
		end;
		BitmapTextArray = array of BitmapText;

		TextData = record
			x : Integer;
			y : Integer;
			size : Longint;
			clr : Color;
			fontType : AnsiString;
			toPrint : String;
		end;
		TextDataArray = array of TextData;

		Button = record
			postion : ItemPostion;
			text : BitmapText;
			valueChk : Integer;
			backgroundColor : Color;
		end;
		ButtonArray = array of Button;

		//
		// Settings types
		//
   	Settings = record
			startingDifficulty : Integer;
			screenWidth : Integer;
			screenHeight : Integer;
			playerName : String;
			ipAddress : String;
		end;

		MessageBox = record
			whiteBox : ItemPostion;
			title : BitmapText;
			buttonData : ButtonArray;
		end;

		//
		// Game Postion Locations
		//
		CurrencyBox = record
			kind : CurrencyKind;
			postion : ItemPostion;
			currencyTitle : TextData;
			currencyAmountTitle : BitmapText;
			currencyAmountPostion : TextData;
			graph : ItemPostion;
			// Seprated due to currencyValue Updateing every 5 or 10 secs
			currencyValueTitle : BitmapTextArray;
			currencyValuePostion : BitmapTextArray;
		end;

		RightBar = record
			postion : ItemPostion;
			currencyBoxTop : CurrencyBox;
			currencyBoxBot : CurrencyBox;
			worldTimerTitle : BitmapText;
			worldTimer : TextData;
		end;

		LeftBar = record
			postion : ItemPostion;
			currencyBoxTop : CurrencyBox;
			currencyBoxBot : CurrencyBox;
		end;

		UserInterfacePostion = record
			rightBarData : RightBar;
			leftBarData : LeftBar;
			scorePostion : TextData;
		end;

		//
		// Game types
		//
		Game = record
			difficulty : Integer;
			settingsData : Settings;
			timeData : Times;
			UserInterfacePostionData : UserInterfacePostion;
			worldData : World;
			marketData : Market;
			enemyData : EnemyArray;
			playerData : Player;
			projectileData : ProjectileArray;
			gameOver : Boolean;
		end;

		//
		// Inputs Enmu
		// REMEBER FOR InSelect It will automatcily increase for every CurrencyKind so be careful
		Inputs = (InScoreBoard = Integer(LeftShiftKey), InUpload = Integer(Tkey), InShoot = Integer(SpaceKey), InSelect = Integer(Key1));

implementation

end.

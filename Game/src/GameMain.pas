{
    Updated by: Paul Sarda
    Date: august 25 2016
    Time Since last Sane 25/08/2016;
    Time Since Happy Trees where added 28/08/2016 11:01:PM
    Every secound > 25/08/2016 && eveysecound < 28/08/2016;
    Start 08:42AM   29/08/2016  :   10:30AM     29/08/2016;
    Start 02:45PM   29/08/2016  :   03:28PM     29/08/2016;
    Start 04:05PM   29/08/2016  :   05:02PM     29/08/2016;
    Start 07:09PM   29/08/2016  :   09:03PM     29/08/2016;
    Start 11:30AM   29/08/2016  :   01:52PM     30/08/2016;
    Start 04:08PM   30/08/2016  :   05:26PM     30/08/2016;
    Start 10:30PM   30/08/2016  :   12:03AM     31/08/2016;
    Start 11:52AM   31/08/2016  :   02:26PM     31/08/2016;
    Start 05:45PM   31/08/2016  :   11:04PM     31/08/2016;
    Start 08:22AM   01/09/2016  :   09:23AM     31/08/2016;
    Start 02:30PM   01/09/2016  :   04:30PM     01/09/2016;
    Start 05:12PM   01/09/2016  :   08:51PM     01/09/2016 //repleaced Value In USD with Buy USD and Sell USD. Created Money Trasnfer. 30% enemy Movement. made Happy Dude;
    Start 10:01PM   01/09/2016  :   11:42PM     01/09/2016 // Removed some repeated code made functions to check if somthing is inside of the gameZone added to enemy movent stil nowhere near complete;
    Start 08:20AM   02/09/2016  :   09:30AM     02/09/2016 // Just spent the whole time trying to understand How Reqtangle works in swingame for DrawText() but it's so strange becuase whenever i put in a Reqtangle it wants a Single whenever i put in a Single it wants a Reqtangle really annoying;
    Start 12:25AM   03/09/2016  :   01:50AM     03/09/2016 // Made enmeys move correctly. Minor Text fixes. made player movement kinda work. still figureing out SellValue;
    start ??????    03/09/2016  :   11:58PM     03/09/2016  // Intergrated a tiny tiny <4 of asm code
    start 08:43AM   05/09/2016  :   11:15PM     05/09/2016  // Split off all the types and colsion stuff into seprate files and worked on said files;
    Start 04:30PM   05/09/2016  :   06:30PM     05/09/2016 // lernt more about assmbly code and made the code i wrote less Shucks;
    Start 08:12AM   06/09/2016  :   11:30AM     06/09/2016
    Start 01:30PM   06/09/2016  :   02:27PM     06/09/2016 //Added nodCurrnency and nod Draw
    Start 11:10PM   06/09/2016  :   12:21AM     07/09/2016 //Wasted the entire time making the backgrounds and fixed currency text from crashing
    Start 02:04AM   07/09/2016  :   02:38AM     07/09/2016 //The way free pascal handles assmbly is so insane there is only one floating point regrester so that's intersteing
    Start 07:23PM   07/09/2016  :   10:31PM     07/09/2016 //Added so when selcted the text of the currency will go red for tranfering from and green for too also complety changed the way the market works
    Start 08:24AM   08/09/2016  :   09:46AM     08/09/2016 //impovred market
    Start 02:30PM   08/09/2016  :   04:30PM     08/09/2016 //revisted asm code updated collisions movemnt is still imposible
    Start 08:35PM   08/09/2016  :   10:30PM     08/09/2016
    Start 08:30PM   09/09/2016  :   10:12PM     09/09/2016
    Start 10:54PM   09/09/2016  :   12:32AM     10/09/2016
    Start 01:15PM   10/09/2016  :   03:30PM     10/09/2016
    Start 09:09PM   11/09/2016  :   12:30PM     12/09/2016
    Start 02:13PM   12/09/2016  :   11:47PM     12/09/2016 //i feel like im in a fight to the death with this program
    Start 03:02PM   13/09/2016  :   09:25PM     13/09/2016 //i decied to re do how market value works compltely
    Start 06:30PM   14/09/2016  :   10:20PM     14/09/2016 //I changed how the market works 100% so that's kinda cool i guess
    Start 09:21AM   15/09/2016  :   10:06AM     15/09/2016 //i made it so amount history updates on chat and added sound effects for enemy spawn and enemy shoot
    Start 11:56AM   15/09/2016  :
    Start 03:57PM   19/09/2016  :
    Start 04:18PM   20/09/2016  :   04:25PM     20/09/2016 //Made damage half work
    Start 11:02PM   20/09/2016  :   02:52AM     21/09/2016 //Added a file where it stores the Resoltuon and stuff and a started to create a settings screen
    Start 02:30PM   21/09/2016  :   06:36PM     21/09/2016 //Settings screen is fully in made made
    Start 08:34PM   21/09/2016  :   10:30PM     21/09/2016 //Updated SturtChart
    Start 02:38PM   22/09/2016  :    N/A
    Start 10:03PM   23/09/2016  :   03:04AM     24/09/2016 //Im finshing the Daming program this weekend Lol As if
    Start 10:28AM   24/09/2016  :   Started Using GitHub

}
program NumbersOrDie;
uses SwinGame, sgTypes, SysUtils, nodTypes, nodMisc, nodScore, nodCollisions, nodCurrency, nodDraw, nodSettings, nodEnemy, nodMarket, nodDelete, nodProjectile, DateUtils;

//
// Asset
//
procedure LoadAudioFiles();
begin

    //
    // Passive Music
    LoadMusicNamed('PassiveBackroundOneMusic','PassiveBackgroundMusic.wav');
    //
    // Chaos Music
    LoadMusicNamed('ChaosBackroundOneMusic','ChaosBackgroundMusic.wav');

    //
    // Spawn Sounds
		// Soruce: My Mouth
    LoadSoundEffectNamed('EnemyTreeSpawnSound', 'EnemyOneSpawn.wav');
    LoadSoundEffectNamed('EnemyImpSpawnSound', 'EnemyTwoSpawn.wav');

    //
    // Shoot Sounds
		// Soruce: My Mouth
    LoadSoundEffectNamed('EnemyShootSound', 'EnemyShoot.wav');
    LoadSoundEffectNamed('PlayerShootSound', 'PlayerShoot.wav');

    //
    // Damage Sound
		// Soruce: My Mouth
    LoadSoundEffectNamed('PlayerDamageSound', 'PlayerDamage.wav');
end;

procedure LoadBitmapFiles();
begin
    //
    // Default bitmap
    LoadBitmapNamed('Default', 'Filler.png');


    // Player Passive
    LoadBitmapNamed('PlayerPassive', 'PlayerPassive.png');
    //
    // Player Chaos
    LoadBitmapNamed('PlayerChaos', 'PlayerChaos.png');

    //
    // Enemy Passive
    LoadBitmapNamed('EnemyTreePassive', 'HappyTree.png');
    LoadBitmapNamed('EnemyImpPassive', 'EnemyTwoPassive.png');
		LoadBitmapNamed('EnemyRDTWPassive', 'EnemyThreeChaos.png');
    //
    // Enemy Chaos
    LoadBitmapNamed('EnemyTreeChaos', 'AngryTree.png');
    LoadBitmapNamed('EnemyImpChaos', 'EnemyTwoChaos.png');
    LoadBitmapNamed('EnemyRDTWChaos', 'EnemyThreeChaos.png');

    //
    //projectiles
    LoadBitmapNamed('ProjectileEnemy', 'ProjectileOne.png');
    LoadBitmapNamed('ProjectilePlayer', 'ProjectileTwo.png');
end;

procedure LoadFont();
begin
    LoadFontNamed('Calibri', 'calibri.ttf', 12);
end;

procedure AssetInitialisation();
begin
    OpenAudio();
    LoadFont();
    LoadAudioFiles();
    LoadBitmapFiles();
end;

//
// Music functions/procedure
//
procedure PlayBackroundMusic(const worldState: WorldKind);
begin
    SetMusicVolume(1);
    if worldState = Passive then
        PlayMusic('PassiveBackroundOneMusic')
    else
        PlayMusic('ChaosBackroundOneMusic');
end;

//
// Player functions/procedure
//
procedure TransferFunds(sell, buy: CurrencyPtr; const currencyValue: MarketValueArray);
var
    SellAmount : Single;

begin
    SellAmount := sell^.amount / 2;
    //adds the converted amount of money to buy
    buy^.amount += CalcCurrencyConversion(SellAmount, currencyValue[Integer(sell^.kind)].buyValue[Integer(buy^.kind)]);
    // minus the amount of money transfed
    sell^.amount -= SellAmount;
end;
//
// Purpose: Creates the player Projectile
//
procedure PlayerShoot(var projectileData: ProjectileArray; var playerData: Player; const selctedCurrency: CurrencyKind; const difficulty: Integer);
var
    prokind : ProjectileKind;
    x, y, xDirection, yDirection, speed : Integer;

begin
    proKind := GetProjectileKind(playerData);
    x := playerData.x + (BitmapWidth(playerData.avatar) >> 1);
    y := playerData.y + (BitmapHeight(playerData.avatar) >> 1);
    speed := GetProjectileSpeed(proKind);
    if playerData.x = playerData.xPrevious then
        xDirection := 0
    else if playerData.x > playerData.xPrevious then
        xDirection := speed
    else
        xDirection := speed * -1;

    if playerData.y = playerData.yPrevious then
        yDirection := 0
    else if playerData.y > playerData.yPrevious then
        yDirection := speed
    else
        yDirection := speed * -1;
    AddProjectileArray(projectileData, x, y, xDirection, yDirection, proKind, selctedCurrency, difficulty, @playerData.currencyOwned[Integer(selctedCurrency)]);
    // Removes the amount the player shot
    playerData.currencyOwned[Integer(selctedCurrency)].amount -= GetProjectileDamage(proKind, difficulty);
    PlaySoundEffect('PlayerShootSound', 0.50);
end;
//
// Purpose: Returns the Player Bitmap
//
function GetPlayerBitmap(const worldState: WorldKind): Bitmap;
begin
    result := BitmapNamed('PlayerPassive');
    if worldState = Passive then
        result := BitmapNamed('PlayerPassive')
    else
        result := BitmapNamed('PlayerChaos');
end;
//
// Purpose: Updates the Amount history for player Currency
//
procedure UpdatePlayerCurrency(var currencyOwned: CurrencyArray; var amountHistoryTimer: Timer);
var
    i : Integer;

begin
    if TimerTicks(amountHistoryTimer) >= 500 then
    begin
        for i:=0 to High(currencyOwned) do
            UpdateAmountHistory(currencyOwned[i].amountHistory, currencyOwned[i].amount);
        ResetTimer(amountHistoryTimer);
    end;
end;
//
// Purpose: Updates the Player record
//
procedure UpdatePlayer(var playerData: Player; const settingsPlayerName: String; var amountHistoryTimer: Timer; var projectileData: ProjectileArray; const marketData: Market);
begin
		if playerData.entityName <> settingsPlayerName then
			playerData.entityName := settingsPlayerName;
    playerData.netWorth := CalculateNetWorth(playerData.currencyOwned, marketData);
    UpdatePlayerCurrency(playerData.currencyOwned, amountHistoryTimer);
end;
//
// Purpose: Sets the Intial Values for the player
//
procedure InitialisePlayer(var playerData: Player; const marketData: Market; const playerName : String; const worldState: WorldKind);
begin
    playerData.entityName := playerName; //Add naming stuff
    InitialiseCurrencyPlayer(playerData.currencyOwned);
    playerData.netWorth := CalculateNetWorth(playerData.currencyOwned, marketData);
    InitialiseCurrencyPtrArray(playerData.selectionData);
    playerData.avatar := GetPlayerBitmap(worldState);
    playerData.x := (ScreenWidth() - BitmapWidth(playerData.avatar)) div 2;
    playerData.y := (ScreenHeight() - BitmapHeight(playerData.avatar)) div 2;
    playerData.speed := START_PLAYER_SPEED;
end;

//
// Time functions/procedure
//
procedure InitialiseTimes(var timeData: Times);
begin
    ReleaseAllTimers();
    timeData.worldTimer := CreateTimer();
    StartTimer(timeData.worldTimer);
    timeData.difficultyTimer := CreateTimer();
    StartTimer(timeData.difficultyTimer);
    timeData.amountHistoryTimer := CreateTimer();
    StartTimer(timeData.amountHistoryTimer);
end;

//
// World Functions/procudres
//

procedure SwitchPlayerState(var playerData: Player; const worldState: WorldKind);
begin
    playerData.avatar := GetPlayerBitmap(worldState);
    if worldState = Passive then
        PlayerData.speed := START_PLAYER_SPEED
    else
        PlayerData.speed := UpdateSpeedChaos(playerData.speed);
end;

procedure SwitchEnemyState(var EnemyData: EnemyArray; const worldState: WorldKind);
var
    i : Integer;

begin
    for i:=0 to High(enemyData) do //I SPENT 30 MINNUITES WONDERING WHY IT WASN'T WORKING I FORGOT BEGIN
    begin
      enemyData[i].speed := GetEnemySpeed(enemyData[i].kind, worldState);
      enemyData[i].avatar := GetEnemyBitmap(enemyData[i].kind, worldState);
    end;
end;

function GetWorldbackgroundColor(const worldState: WorldKind): Color;
begin
    result := ColorWhite;
    if worldState = Passive then
        result := ColorDarkGreen
    else
        result := ColorDarkRed;
end;

procedure SwitchWorldState(var data: Game);
begin
    if data.worldData.worldState = Passive then
        data.worldData.worldState := Chaos
    else
        data.worldData.worldState := Passive;

    data.worldData.backgroundColor := GetWorldbackgroundColor(data.worldData.worldState);
    ResetTimer(data.timeData.worldTimer);
    PlayBackroundMusic(data.worldData.worldState);

    SwitchEnemyState(data.enemyData, data.worldData.worldState);
    SwitchPlayerState(data.playerData, data.worldData.worldState);
end;

procedure UpdateWorld(var data: Game);
begin
    if (data.worldData.worldState = Passive) and (TimerTicks(data.timeData.worldTimer) > TIME_PASSIVE_STATE) then
        SwitchWorldState(data)
    else if (data.worldData.worldState = Chaos) and (TimerTicks(data.timeData.worldTimer) > TIME_CHAOS_STATE) then
        SwitchWorldState(data);
end;


procedure InitialiseWorld(var worldData: World);
begin
    worldData.worldState := Passive;
    worldData.backgroundColor := GetWorldbackgroundColor(worldData.worldState);
    PlayBackroundMusic(worldData.worldState);
end;

//
// difficulty Functions/procedure
//
procedure InitialiseDifficulty(var difficulty: Integer; startingDifficulty: Integer);
begin
    difficulty := startingDifficulty;
end;

procedure UpdateDifficulty(var difficulty: Integer; var difficultyTimer: Timer; const worldState: WorldKind);
begin
    if worldState = Passive then
    begin
        difficulty += TimerTicks(difficultyTimer) >> 5

    end
    else
        difficulty += TimerTicks(difficultyTimer) div 50;
    ResetTimer(difficultyTimer);
end;

//
// Inputs
//

procedure CheckGameOver(var gameOver : Boolean; const netWorth: Single);
begin
    if (netWorth <= 0) then
        gameOver := true;
    if KeyReleased(Rkey) or KeyReleased(KeyCode(InUpload)) then
        gameOver := true;
end;

procedure HandlePlayerShoot(var projectileData: ProjectileArray; var playerData: Player; const difficulty: Integer);
begin
    if KeyReleased(KeyCode(InShoot)) then
				// Checks that player has moved
        if (not(playerData.xPrevious = playerData.x)) or (not(playerData.yPrevious = playerData.y)) then
						//Checks that player has money to shoot
            if (playerData.selectionData[0] <> nil) and (playerData.currencyOwned[Integer(playerData.selectionData[0]^.kind)].amount > 0) then
                PlayerShoot(projectileData, playerData, playerData.selectionData[0]^.kind, difficulty);
end;

procedure HandlePlayerMovementInput(var playerData: Player; const enemyData: EnemyArray);
begin
    playerData.xPrevious := playerData.x;
    playerData.yPrevious := playerData.y;
    if not CheckCollisionAgainstArray(playerData.x, playerData.y, -1, playerData.avatar, enemyData) then
    begin
        if KeyDown(UpKey) and (playerData.y > 0) then
            playerData.y -= playerData.speed;
        if KeyDown(DownKey) and (playerData.y + BitmapHeight(playerData.avatar) < ScreenHeight()) then
            playerData.y += playerData.speed;
        if KeyDown(RightKey) and (playerData.x + BitmapWidth(PlayerData.avatar) < XSideBarLocation()) then
            playerData.x += playerData.speed;
        if KeyDown(LeftKey) and (playerData.x > CalcSideBarWidth()) then
            playerData.x -= playerData.speed;
    end
    else
    begin
        if not CheckCollisionAgainstArray(playerData.x - playerData.speed, playerData.y, -1, playerData.avatar, enemyData) then
            playerData.x -= playerData.speed
        else
            playerData.x += playerData.speed;
        if not CheckCollisionAgainstArray(playerData.x, playerData.y - playerData.speed, -1, playerData.avatar, enemyData) then
            playerData.y -= playerData.speed
        else
            playerData.y += playerData.speed;
    end;
end;

procedure HandlePlayerSelectionInput(var transfer: CurrencyPtrArray; var currencyOwned: CurrencyArray; const currencyValue: MarketValueArray);
var
    i, k : Integer;

begin
    // incremnets the index for transfer so it updates the unassigned entry
    i := 0;
    while (transfer[i] <> nil) and (i < High(transfer)) do
        i+=1;
    // Checks if key1 or anything passed that based on the amount of currencys
    for k:=0 to Integer(CurrencyKindLength)-1 do
        if KeyReleased(KeyCode(k + Integer(InSelect))) then
            transfer[i] := @currencyOwned[k];
    // clears all selected Currencys
    if KeyReleased(CapsLockKey) then
        InitialiseCurrencyPtrArray(transfer)
    // Transfer Currency in the first postion to the amonut of the 2nd selceted currency
    else if KeyReleased(TabKey) then
    begin
        if (transfer[0] <> nil) and (transfer[1] <> nil) then
            TransferFunds(transfer[0], transfer[1], currencyValue);
        InitialiseCurrencyPtrArray(transfer);
    end;
end;

procedure HandlePlayerInput(var playerData: Player; var projectileData: ProjectileArray; const enemyData: EnemyArray; const currencyValue: MarketValueArray; const difficulty: Integer);
begin
    HandlePlayerSelectionInput(playerData.selectionData, playerData.currencyOwned, currencyValue);
    HandlePlayerMovementInput(playerData, enemyData);
    HandlePlayerShoot(projectileData, playerData, difficulty);
end;

procedure HandleInput(var gameData : Game);
begin
    HandlePlayerInput(gameData.playerData, gameData.projectileData, gameData.enemyData, gameData.marketData.currencyValue, gameData.difficulty);
    if KeyReleased(EscapeKey) then
      OpenSettingsScreen(gameData.settingsData, gameData.timeData);
		if KeyReleased(KeyCode(InScoreBoard)) then
			OpenScoreScreen(gamedata.settingsData.ipAddress, gameData.timeData);
    if KeyReleased(EKey) and (gameData.worldData.worldState = Passive) then
      SwitchWorldState(gameData);
end;

//
// GameLoop
//
procedure CheckDifficultyTimer(var gamedata: Game);
begin
    // checks if it's time to update
    if ((gameData.worldData.worldState = Passive) and (TimerTicks(gameData.timeData.difficultyTimer) > DIFFICULTY_TIMER_LOOP)) or ((gameData.worldData.worldState = Chaos) and (TimerTicks(gameData.timeData.difficultyTimer) > DIFFICULTY_TIMER_LOOP div 2)) then
    begin
        UpdateDifficulty(gameData.difficulty, gameData.timeData.difficultyTimer, gameData.worldData.worldState);
        UpdateMarket(gameData.marketData, gameData.UserInterfacePostionData, gameData.playerData, gameData.enemyData, gameData.worldData.worldState);
    end;
end;

procedure CheckScreenSizeChanged(var UserInterfacePostionData: UserInterfacePostion; const settingsData: Settings; const currencyValue: MarketValueArray);
begin
	if not (ScreenWidth() = settingsData.screenWidth) then
	begin
		ChangeScreenSize(settingsData.ScreenWidth, settingsData.ScreenHeight);
		InitialiseUserInterfacePostion(UserInterfacePostionData);
		UpdateAllValuesPostionsArray(UserInterfacePostionData, currencyValue);
	end;

end;

procedure UpdateGame(var gameData: Game);
begin
    UpdateWorld(gameData);
		CheckGameOver(gamedata.gameOver, gamedata.playerData.netWorth);
    CheckDifficultyTimer(gamedata);
    UpdatePlayer(gameData.playerData, gamedata.settingsData.playerName, gameData.timeData.amountHistoryTimer, gameData.projectileData, gameData.marketData);
    UpdateEnemyArray(gameData.enemyData,  gameData.projectileData, gameData.playerData, gameData);
    UpdateProjectileArray(gameData.projectileData, gameData.playerData, gameData.enemyData, gameData.marketData.currencyValue);
		CheckScreenSizeChanged(gameData.UserInterfacePostionData, gameData.settingsData, gamedata.marketData.currencyValue);
end;

procedure GameLoop(var gameData: Game);
begin
    HandleInput(gameData);
    UpdateGame(gameData);
    DrawGame(gameData);
end;

//
// Game Functions/ procedure
//

procedure InitialiseGame(var data: Game);
begin
    // sets gameover as false
    data.gameOver := false;
    InitialiseDifficulty(data.difficulty, data.settingsData.startingDifficulty);
    InitialiseTimes(data.timeData);
    InitialiseWorld(data.worldData);
    InitialiseProjectile(data.projectileData);
    InitialiseMarket(data.marketData, data.UserInterfacePostionData, data.difficulty);
    InitialiseEnemy(data.enemyData);
    InitialisePlayer(data.playerData, data.marketData, data.settingsData.playerName, data.worldData.worldState);
end;

procedure Main();
var
    gameData : Game;

begin
    //
    // Initialise calls
    AssetInitialisation();
    InitialiseSettings(gameData.settingsData);
    OpenGraphicsWindow('NUMBERS OR DIE', gameData.settingsData.screenWidth, gameData.settingsData.screenHeight);
    InitialiseUserInterfacePostion(gameData.UserInterfacePostionData);
    repeat
        InitialiseGame(gameData);
        repeat
            ProcessEvents();
            GameLoop(gameData);
            DrawFramerate(0,0);
            RefreshScreen();
            // gameData.gameOver := true;
        until (gameData.gameOver) or WindowCloseRequested();
				if KeyReleased(KeyCode(InUpload)) then
					OpenUploadScreen(gameData.playerData.netWorth, gameData.playerData.entityName, gameData.settingsData.ipAddress);
    until WindowCloseRequested();
end;

begin
  Main();
end.

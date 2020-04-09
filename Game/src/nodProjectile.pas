unit nodProjectile;

interface
	uses SwinGame, sgTypes, nodTypes, nodDelete, nodDamage, nodMisc, nodCollisions;
	//
	// Projectile function/procedure
	//
	function GetProjectileBitmap(const proKind: ProjectileKind): Bitmap;

	function GetProjectileDamage(const proKind: ProjectileKind; const difficulty: Integer): Integer;

	function GetProjectileSpeed(const proKind: ProjectileKind): Integer;

	function AddProjectile(x, y, xDirection, yDirection: Integer; proKind: ProjectileKind; const damageKind: CurrencyKind; const difficulty: Integer; const owner: CurrencyPtr): Projectile;

	procedure AddProjectileArray(var projectileData: ProjectileArray; x, y, xDirection, yDirection: Integer; proKind: ProjectileKind; const damageKind: CurrencyKind; const difficulty: Integer; const owner: CurrencyPtr);

	function MoveProjectile(var projectileData: Projectile): Boolean;

	function UpdateProjectile(var projectileData: Projectile; var playerData: Player; var enemyData: EnemyArray; const currencyValue: MarketValueArray): Boolean;

	procedure UpdateProjectileArray(var projectileData: ProjectileArray; var playerData: Player; var enemyData: EnemyArray; const currencyValue: MarketValueArray);

	procedure InitialiseProjectile(var projectileData: ProjectileArray);

	function GetProjectileKind(const playerData: Player): ProjectileKind; OverLoad;

implementation
	//
	// Returns the appropriate bitmap for a given ProjectileKind
	//
	function GetProjectileBitmap(const proKind: ProjectileKind): Bitmap;
	begin
		result := BitmapNamed('Default');
		case proKind of
			ProjectileEnemy : result := BitmapNamed('ProjectileEnemy');
			ProjectilePlayer : result := BitmapNamed('ProjectilePlayer');
		end;
	end;
	//
	// Returns the appropriate damage for a given ProjectileKind
	//
	function GetProjectileDamage(const proKind: ProjectileKind; const difficulty: Integer): Integer;
	begin
		result := 0;
		case proKind of
			ProjectileEnemy : result := difficulty div 30;
			ProjectilePlayer : result := 200;
		end;
	end;
	//
	// Returns the appropriate speed for a given ProjectileKind
	//
	function GetProjectileSpeed(const proKind: ProjectileKind): Integer;
	begin
		result := 0;
		case proKind of
			ProjectileEnemy : result := 20;
			ProjectilePlayer : result := 13;
		end;
	end;
	//
	// Returns an projetile using the given projectileKind and the damage type
	//
	function AddProjectile(x, y, xDirection, yDirection: Integer; proKind: ProjectileKind; const damageKind: CurrencyKind; const difficulty: Integer; const owner: CurrencyPtr): Projectile;
	begin
		result.proKind := proKind;
		result.damageKind := damageKind;
		result.owner := owner;
		result.avatar := GetProjectileBitmap(result.proKind);
		result.damage := GetProjectileDamage(result.proKind, difficulty);
		result.speed := GetProjectileSpeed(result.proKind);
		result.x := x;
		result.y := y;
		result.xDirection := xDirection;
		result.yDirection := yDirection;
	end;
	//
	// increases size of projectile Array then adds a new
	//
	procedure AddProjectileArray(var projectileData: ProjectileArray; x, y, xDirection, yDirection: Integer; proKind: ProjectileKind; const damageKind: CurrencyKind; const difficulty: Integer; const owner: CurrencyPtr);
	begin
		SetLength(projectileData, Length(projectileData)+1);
		projectileData[High(projectileData)] := AddProjectile(x, y, xDirection, yDirection, proKind,damageKind, difficulty, owner);
	end;

	function MoveProjectile(var projectileData: Projectile): Boolean;
	begin
		result := false;
		if (projectileData.x > XSideBarLocation()) or (projectileData.x < 0) then
			result := true
		else
			projectileData.x += projectileData.xDirection;

		if (projectileData.y > ScreenHeight()) or (projectileData.y < 0) then
			result := true
		else
			projectileData.y += projectileData.yDirection;
	end;

	function UpdateProjectile(var projectileData: Projectile; var playerData: Player; var enemyData: EnemyArray; const currencyValue: MarketValueArray): Boolean;
	var
		i : Integer;
		temp : CurrencyKind;

	begin
		result := false;
		result := MoveProjectile(projectileData);
		if projectileData.proKind =  ProjectileEnemy then
		begin
			if CheckPlayerCollision(projectileData.x, projectileData.y, projectileData.avatar, playerData) then
			begin
				AllocateDamage(playerData.currencyOwned[Integer(projectileData.damageKind)].amount, projectileData.owner^.amount, projectileData.damage);
				result := true;
			end;
		end
		else if projectileData.proKind =  ProjectilePlayer then
		begin
			for i:=0 to High(enemyData) do
				if BitmapCollision(projectileData.avatar, projectileData.x, projectileData.y, enemyData[i].avatar, enemyData[i].x, enemyData[i].y) then
				begin
					//Enemy Three cannot take damage
					if (enemyData[i].kind = EnemyTree) or (enemyData[i].kind = EnemyImp) then
						AllocateDamage(enemyData[i].currencyOwned[Integer(projectileData.damageKind)].amount, projectileData.owner^.amount, projectileData.damage);
					result := true;
				end;
		end;
	end;

	procedure UpdateProjectileArray(var projectileData: ProjectileArray; var playerData: Player; var enemyData: EnemyArray; const currencyValue: MarketValueArray);
	var
		i, j : Integer;
		toDelete : IntegerArray;

	begin
		for i:=0 to High(projectileData) do
			// Returns True if the projecitle needs to be delted
			if UpdateProjectile(projectileData[i], playerData, enemyData, currencyValue) then
			begin
				// checks if the toDelte array has been insitlsied
				if Length(toDelete) = 0 then
				begin
					SetLength(toDelete, 1);
					j := 0;
				end
				else
					SetLength(toDelete, Length(toDelete)+1);
				toDelete[j] := i;
				j += 1;
				// WriteLn('ToDeltePro:', Length(toDelete));
			end;

		for i:= 0 to High(toDelete) do
			DeleteArrayElement(projectileData, toDelete[i]);
	end;

	procedure InitialiseProjectile(var projectileData: ProjectileArray);
	begin
		SetLength(projectileData, 0);
	end;

	//
	// returns a projetile kind based on paramrents
	function GetProjectileKind(const kind: EnemyKind): ProjectileKind; OverLoad;
	begin
		result := ProjectileEnemy;
		case kind of
			EnemyTree : result := ProjectileEnemy;
			EnemyImp : result := ProjectileEnemy;
		end;
	end;

	function GetProjectileKind(const playerData: Player): ProjectileKind; OverLoad;
	begin
		result := ProjectilePlayer;
	end;
end.

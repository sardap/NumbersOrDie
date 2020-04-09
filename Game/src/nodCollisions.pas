unit nodCollisions;

interface
	uses SwinGame, sgTypes, nodTypes;

	//
	// Check Aganst array
	//

	//
	// Checks vs enemyArray
	function CheckCollisionAgainstArray(const x, y: Integer; iSkip: Integer; const avatar: Bitmap; const enemyData: EnemyArray): Boolean; OverLoad;

	//
	// Check Player
	//
	function CheckPlayerCollision(const x, y: Integer; const avatar: Bitmap; const playerData: Player): Boolean;

implementation
	//
	// Check Aganst array
	//

	//
	// Checks vs enemyArray
	function CheckCollisionAgainstArray(const x, y: Integer; iSkip: Integer; const avatar: Bitmap; const enemyData: EnemyArray): Boolean; OverLoad;
	var
		i : Integer;

	begin
		result := false;
		for i:=0 to High(enemyData) do
			if i <> iSkip then
				if BitmapCollision(avatar, x, y, enemyData[i].avatar, enemyData[i].x, enemyData[i].y) then
					result := true;

	end;

	//
	// Check Player
	//

	//
	// check aganst player
	function CheckPlayerCollision(const x, y: Integer; const avatar: Bitmap; const playerData: Player): Boolean;
	begin
		result := BitmapCollision(avatar, x, y, playerData.avatar, playerData.x, playerData.y);
	end;
end.

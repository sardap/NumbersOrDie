unit nodDelete;

interface
	uses nodTypes;
	//
	// Delete Functions/procedures
	//
	procedure DeleteArrayElement(var projectileData: ProjectileArray; idx: Integer); OverLoad;


	procedure DeleteArrayElement(var enemyData: EnemyArray; idx: Integer); OverLoad;

implementation
	//
	// Delete Functions/procedures
	//
	procedure DeleteArrayElement(var projectileData: ProjectileArray; idx: Integer); OverLoad;
	var
		i: Integer;

	begin
		for i := idx to High(projectileData) - 1 do
			projectileData[i] := projectileData[i+1];
		SetLength(projectileData, Length(projectileData) - 1);
	end;

	procedure DeleteArrayElement(var enemyData: EnemyArray; idx: Integer); OverLoad;
	var
		i: Integer;

	begin
		for i := idx to High(enemyData) - 1 do
			enemyData[i] := enemyData[i+1];
		SetLength(enemyData, Length(enemyData) - 1);
	end;
end.

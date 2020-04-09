unit nodDamage;

interface
    uses SwinGame, sgTypes, nodTypes;
    //
    // Damage Functions/Procedure
    //

    //
    // allocates damage
    procedure AllocateDamage(var taking, gaining: Single; damage: Single); OverLoad;
    //
    // allocates damage WITH SOUND
    procedure AllocateDamage(var taking, gaining: Single; damage: Single; const sound: SoundEffect); OverLoad;

implementation
    //
    // Damage Functions/Procedure
    //

    //
    // allocates damage
    procedure AllocateDamage(var taking, gaining: Single; damage: Single); OverLoad;
    begin
        taking -= damage;
        gaining += damage * 1.10;
    end;

    //
    // allocates damage WITH SOUND
    procedure AllocateDamage(var taking, gaining: Single; damage: Single; const sound: SoundEffect); OverLoad;
    begin
        AllocateDamage(taking, gaining, damage);
        PlaySoundEffect(sound, 0.20);
    end;
end.

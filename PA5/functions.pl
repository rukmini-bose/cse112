/*
* Airport Database.
* For each airport:
* - three-letter airport code
* - name of city
* - north latitude: degrees and minutes
* - west longitude: degrees and minutes
* North latitudes and West longitudes are in degrees, minutes.
*/

airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,56 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,18 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).


/*
* Flight schedule.
* Departure airport, destination airport,
* departure time in hours, minutes.
*/

flight( bos, nyc, time(  7,30 ) ).
flight( dfw, den, time(  8, 0 ) ).
flight( atl, lax, time(  8,30 ) ).
flight( chi, den, time(  8,30 ) ).
flight( mia, atl, time(  9, 0 ) ).
flight( sfo, lax, time(  9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 0 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).

% ----------------------------------------------------------------------
% Display Rules
% ----------------------------------------------------------------------

write_time( Time ) :-
  (Time < 10, write(0), write( Time )) ;
  (Time >= 10, write( Time )).

show_leg(Type, Code, H, M) :-
  write(Type), write('  '),
  upcase_atom(Code, Up), write(Up), write('  '),
  airport(Code, Name, _, _), write(Name), write('  '),
  write_time(H), write(':'), write_time(M),
  nl.

% ----------------------------------------------------------------------
% Math Rules
% ----------------------------------------------------------------------

radians(Deg, Min, Rad) :-
  Rad is ((Deg + (Min / 60.0)) * pi) / 180.0.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

dist(Dep, Arr, Dist) :-
  airport(Dep, _, degmin(DegA1, MinA1), degmin(DegO1, MinO1)),
  airport(Arr, _, degmin(DegA2, MinA2), degmin(DegO2, MinO2)),
  radians(DegA1, MinA1, RadA1),
  radians(DegO1, MinO1, RadO1),
  radians(DegA2, MinA2, RadA2),
  radians(DegO2, MinO2, RadO2),
  haversine_radians(RadA1, RadO1, RadA2, RadO2, Dist).

land_time(Dist, StartH, StartM, LandH, LandM) :-
  StartRaw is StartH * 60 + StartM,
  LandRaw is StartRaw + round(Dist * 60 / 500),
  LandH is floor(LandRaw / 60),
  LandM is LandRaw mod 60.

time_greater(ThisH, ThisM, OtherH, OtherM) :-
  (ThisH > OtherH) ;
  (ThisH = OtherH , ThisM >= OtherM).

% ----------------------------------------------------------------------
% Pathing Rules
% ----------------------------------------------------------------------

not( X ) :- X, !, fail.
not( _ ).

fly_once(Dep, Arr,
    StartH, StartM,
    EndH, EndM,
    AfterH, AfterM) :-
  flight( Dep, Arr, time(StartH, StartM) ),
  time_greater( StartH, StartM, AfterH, AfterM ),
  dist( Dep, Arr, Dist ),
  land_time( Dist, StartH, StartM, EndH, EndM ).

% ----------------------------------------------------------------------
% Main Entrypoint
% ----------------------------------------------------------------------
  
fly(Dep, Arr) :-
  % dist(Dep, Arr, Dist),
  fly_once(Dep, Arr, StartH, StartM, EndH, EndM, 0, 0),
  show_leg('depart', Dep, StartH, StartM),
  show_leg('arrive', Arr, EndH, EndM).


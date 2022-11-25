type ZamowienieUslugi = {lokalizacja:string; predkoscOczekiwana:int}

type ZamowieniePrzyjete = {KlientID:int;odleglosc:float;zamowienie:ZamowienieUslugi}

type WarunkiPotwierdzoneZdalnie = {predkosc:int;zamowienie:ZamowieniePrzyjete}

type DzialaniePotwierdzone = {predkosc:int; warunki:WarunkiPotwierdzoneZdalnie}

type UslugaUruchomiona = {klientID:int;lokalizacja:string;predkosc:int;abonament:int}

let zamowienie1 = {lokalizacja = "Gdynia"; predkoscOczekiwana = 100}
let zamowienie2 = {lokalizacja = "Gdańsk"; predkoscOczekiwana = 60}
let zamowienie3 = {lokalizacja = "Kartuzy"; predkoscOczekiwana = 20}


#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream>
#include <fstream>
#include <cctype>


//CLASS EQDATA Stores data for one piece of eq
class EqData
{
	private:
	int hp, mana, hr, dr, ss;
	bool dh, qo;
	std::string name;
	EqData *next;

	public:


	EqData()
	{
		hp = 0;
		mana = 0;
		hr = 0;
		dr = 0;
		ss = 0;
		dh = false;
		qo = false;
		name = "";
		next = 0;
	}//End construct

	//same format as input
	EqData(int Hp, int Mana, int Hr, int Dr, int Ss, std::string Name, bool Dh, bool Qo)
	{
		//No eq can be both DH and QO
		assert(!(Dh == true && Qo == true));
		
		hp = Hp; 
		mana = Mana; 
		hr = Hr; 
		dr = Dr; 
		ss = Ss; 
		name = Name; 
		dh = Dh; 
		qo = Qo;
		next = 0;

	}//End construct	

	EqData *getNext()
	{
		return next;
	}

	void setNext(EqData *Next)
	{
		next = Next;
	}

	int getHr()
	{
		return hr;
	}	

	int getDr()
	{
		return dr;
	}	

	int getHp()
	{
		return hp;
	}

	int getMana()
	{
		return mana;
	}

	int getSs()
	{
		return ss;
	}

	std::string getName()
	{
		return name;
	}

	bool isDh()
	{
		return dh;
	}

	bool isQo()
	{
		return qo;
	}

	void setHp(int Hp)
	{
		hp = Hp;
	}

	void setMana(int Mana)
	{
		mana = Mana;
	}

	void setHr (int Hr)
	{
		hr = Hr;
	}

	void setDr (int Dr)
	{
		dr = Dr;
	}

	void setSs (int Ss)
	{
		ss = Ss;
	}

	void setName(std::string Name)
	{
		name = Name;
	}

	void setDh(bool Dh)
	{
		dh = Dh;
	}

	void setQo(bool Qo)
	{
		qo = Qo;
	}
};//END EqData Obj	


//Class EqNode

//CLASS LocData
class LocData
{
	private:
	std::string name;
	EqData *firstNode;
	int num;

	public:

	LocData()
	{
		name = "";
		firstNode = 0;
		num = 0;
	}	

	LocData(std::string Name)
	{
		name = Name;
		firstNode = 0;
		num = 0;
	}	

	LocData(std::string Name, EqData *FirstNode)
	{
		name = Name;
		firstNode = FirstNode;
		num = 0;
	}

	std::string getName()
	{
		return name;
	}

	void setName(std::string Name)
	{
		name = Name;
	}

	EqData *getNext()
	{
		return firstNode;
	}

	void setNext(EqData *FirstNode)
	{
		firstNode = FirstNode;
	}

	void setAmt(int amt)
	{
		num = amt;
	}

	int getAmt()
	{
		return num;
	}
	
};//END LocData Obj	


//GLOBALS

//Wether or not wrist and finger use two input files
bool TWO_FILES;

//The final solution
std::string solution;

//Current set variables
int setHp, setMana, setHr, setDr, setSs, setDh, setQo;
int hpWeight, manaWeight, hrWeight, drWeight, ssWeight;

//Set constraints
int hpMin, manaMin, hrMin, drMin, ssMin, ssLimit;

//Current set points total
int points;

//Character bases
int hpBase, manaBase;

//Names of eq in solution set
static std::string setNames[16];

//Constants
const float VERSION = 1.2;
const int MAX_DH = 2, MAX_QO = 1;
const int NUMBER_OF_LOCATIONS = 16;


//Prototypes:
std::string toLower(std::string str);
LocData loadLoc(std::string location);
void display (EqData *eq);
LocData *getAllEq();
void analyze(LocData *locations, int currLoc);
int getCombinations(LocData *locations);
std::string locationToName(int loc);


int main (int argc, char *argv[])
{
	//Parse command line:
	
	//Initialize TWO_FILES
	TWO_FILES = false;
	
	//To tell if we used custom weights
	bool weightsDefined = false;
	
	//Iterate through the command line args
	for (int x = 1 ; x <= argc - 1 ; x++ )
	{
		std::string nextArg = argv[x];

		if (nextArg.compare("t") == 0)
		{
			hpWeight = 1;
			manaWeight = 0;
			hrWeight = 0;
			drWeight = 8;
			ssWeight = 0;
			weightsDefined = true;
		}
		else if (nextArg.compare("w") == 0)
		{
			hpWeight = 1;
			manaWeight = 0;
			hrWeight = 0;
			drWeight = 8;
			ssWeight = 0;
			weightsDefined = true;
		}
		else if (nextArg.compare("c") == 0)
		{
			hpWeight = 1;
			manaWeight = 1;
			hrWeight = 0;
			drWeight = 0;
			ssWeight = 0;
			weightsDefined = true;
		}
		else if (nextArg.compare("m") == 0)
		{
			hpWeight = 0;
			manaWeight = 1;
			hrWeight = 0;
			drWeight = 0;
			ssWeight = 0;
			weightsDefined = true;
		}
		else if (nextArg.compare("h") == 0)
		{
			hpWeight = 1;
			manaWeight = 0;
			hrWeight = 0;
			drWeight = 0;
			ssWeight = 0;
			weightsDefined = true;
		}
		else if (nextArg.compare("f") == 0)
		{
			TWO_FILES = true;
		}
		else if (nextArg.compare("a") == 0)
		{
			if (argc < x + 6 )
			{
				std::cout << "Error: not enough custom weights specified - see readme" << std::endl;
				exit(0); 
			}
			hpWeight = atoi(argv[x+1]);
			manaWeight = atoi(argv[x+2]);
			hrWeight = atoi(argv[x+3]);
			drWeight = atoi(argv[x+4]);
			ssWeight = atoi(argv[x+5]);
			weightsDefined = true;
			x += 5;
		}
		else
		{
			std::cout << "Error:: invalid command arg " << nextArg << std::endl;
			exit(0);
		}	

	}//End cmd line args for

	if (!weightsDefined)
	{
		std::cout << "Error: no weights defined - see readme" << std::endl;
		exit(0);
	}

	//Load up the eq
	LocData *locations = getAllEq();

	//Initialize starting set values
	setHp = 300; //Spirit + Aura + 2 HP tallies
	setMana = 100; //Spirit + Aura 
	setHr = 11; //Bless strength
	setDr = 18; //strength tallies
	setSs = 2; //Bless + Spirit
	setDh = 0;
	setQo = 0;

	//Main input

	std::cout << "EqOptimizer v" << VERSION << " by Spiritic." << std::endl;

	std::cout << "Using weights:: Hp: " << hpWeight << " Mana: " << manaWeight << " Hr: " << hrWeight << " Dr: " << drWeight << " Ss: " << ssWeight << std::endl;
	
	std::cout << "Using base:: Hp: " << setHp << " Mana: " << setMana << " Hr: " << setHr << " Dr: " << setDr << " ss: -" << setSs << std::endl;

	std::cout << std::endl << "Input the lowest hp you want in your set: ";

	std::cin >> hpMin;

	std::cout << "Input your character's base hp, INCLUDING hp bonus: ";

	std::cin >> hpBase;

	std::cout << "Input the lowest mana you want in your set INCLUDING aura: ";

	std::cin >> manaMin;

	std::cout << "Input your character's base mana: ";

	std::cin >> manaBase;

	std::cout << "Input the lowest dr you want in your set: ";

	std::cin >> drMin;

	std::cout << "Input the lowest hr you want in your set: ";

	std::cin >> hrMin;

	std::cout << "Input the smallest amount of ss you want in your set including bless." << std::endl;

	std::cout << "(Positive number ie -3ss = input 3) MINIMUM 1 FOR BLESS: ";

	std::cin >> ssMin;

	std::cout << "Input the maximum amount of ss you want in your set (if you inputted 3 before," << std::endl;

	std::cout << "input something like 5 now): ";

	std::cin >> ssLimit;




	//Initialize points
	points = 0;

	//Add base to initial values
	setHp += hpBase;
	setMana += manaBase;

	//Display combinations
	if (getCombinations(locations) != 0 )
	{
		std::cout << "Total number of combinations: " << getCombinations(locations) << std::endl << "Calculating set..." << std::endl;
	}

	//Analyze the eq
	analyze(locations, 0);
	
	if (solution.compare("") == 0)
	{
		std::cout << "Eq set is not possible with given eq, try again with different parameters." << std::endl;
	}
	else
	{
		
		std::cout << solution << std::endl;

		std::ofstream output ("solution.txt");
		output << solution;
		output.close();
	}

}//End main	

//pre: 0 <= loc <= 15
//post: returns text representation of the location
std::string locationToName(int loc)
{
	switch(loc)
	{
		case 0:
			return "light";
		case 1:
			return "finger one";
		case 2:
			return "finger two";
		case 3:
			return "body";
		case 4:
			return "head";
		case 5:
			return "legs";
		case 6:
			return "feet";
		case 7:
			return "hands";
		case 8:
			return "arms";
		case 9:
			return "shield";
		case 10:
			return "about";
		case 11:
			return "waist";
		case 12:
			return "wrist one";
		case 13:
			return "wrist two";
		case 14:
			return "wield";
		case 15:
			return "held";
		default:
			return "invalid location";
		
	}//End switch	
	
}//End locationToName	


//pre: locations initialized
//post: returns total number of combinations of items, and stops program if a location has no items
int getCombinations(LocData *locations)
{
	int combinations = 1;

	for (int x = 0; x < NUMBER_OF_LOCATIONS ; x++ )
	{
		combinations *= locations[x].getAmt();
	}

	return combinations;
	
}//End getCombinations	

//pre: eq object initialized
//post: displays eq data on screen
void display (EqData *eq)
{
	std::cout << eq->getHp() << std::endl << eq->getMana() << std::endl;
	std::cout << eq->getHr() << std::endl << eq->getDr() << std::endl;
	std::cout << eq->getSs() << std::endl << eq->getName() << std::endl;
	std::cout << eq->isDh() << std::endl << eq->isQo() << std::endl;
	std::cout << std::endl;
	
}	

//pre: none
//post: returns lowercase str
std::string toLower(std::string str)
{
	const char *cstr = str.c_str();

	std::string toReturn;

	for (int x = 0; x < str.length() ; x++ )
	{
	   toReturn += std::tolower(*(cstr++));
		 
	} 

	return toReturn;
	
}//End toLower	


void analyze(LocData *locations, int currLoc)
{
	
	if (locations[currLoc].getNext() == 0)
	{
		std::cout << "Error:: location " << locationToName(currLoc) << " has no eq." << std::endl;
		exit(0);
	}

	//Cycle through eq within the current location
	EqData *eq = locations[currLoc].getNext();

	while (eq != NULL)
	{
		
		
		//If the piece of eq is DH or QO, increment the counters
		if (eq->isDh())
		{
			setDh++;
		}
		if (eq->isQo())
		{
			setQo++;
		}

		//Now check to see if we have exceeded our quota, and if we have, decrement the counters and skip this eq
		if (setDh <= MAX_DH && setQo <= MAX_QO)
		{
			//Set the eq name
			setNames[currLoc] = eq->getName();
			
			//Add the current piece of eq to the set
			setHp += eq->getHp();
			setMana += eq->getMana();
			setHr += eq->getHr();
			setDr += eq->getDr();
			setSs += eq->getSs();

			//If we have reached the end of the eq list
			if (currLoc >= NUMBER_OF_LOCATIONS - 1)
			{
				//Check to see if the current set is the best, and follows the constraints.
				if (setHp >= hpMin && setMana >= manaMin && setHr >= hrMin && setDr >= drMin && setSs >= ssMin && setSs <= ssLimit)
				{
					int newPoints = setHp * hpWeight + setMana * manaWeight + setHr * hrWeight + setDr * drWeight + setSs * ssWeight;
					if (newPoints > points )
					{
						points = newPoints;
						
						//Build the solution
						solution = "";
						for (int x = 0; x < NUMBER_OF_LOCATIONS ; x++)
						{
							solution += setNames[x] + "\n";
						}//End solution for

						char temp[100];
						sprintf (temp, "%s%d%s%d", "Hp Base: ", hpBase, " Mana Base: ", manaBase); 
						solution += temp;
						solution += "\n";
						sprintf (temp, "%s%d%s%d%s%d%s%d%s%d", "Hp: " , setHp , " Mana: " , setMana , " Hr: " , setHr , " Dr: " , setDr , " Ss: " , setSs); 
						solution += temp;
						solution += "\n";

						 
					}//End if new set better than old set
					
				}//End set passes constraints if
				
			}//End if we have reached the end of the location array
			//Else, recurse to next location
			else
			{
				analyze(locations, currLoc + 1);
			}	

			//Subtract the current piece of eq from the set
			setHp -= eq->getHp();
			setMana -= eq->getMana();
			setHr -= eq->getHr();
			setDr -= eq->getDr();
			setSs -= eq->getSs();


			 
		}//End valid DH or QO piece (analyze body)

		//Once we are done with this piece of eq, subtract it from Dh/Qo if needed
		if (eq->isDh())
		{
			setDh--;
		}
		if (eq->isQo())
		{
			setQo--;
		}

		//Get the next piece of eq
		eq = eq->getNext();

		 
	}//End eq loop
	
}//End analyze	


//pre: none
//post: returns array of all eq in LocData objects
LocData *getAllEq()
{
	//Generate array of LocData objects for all locations
	
	//Locations are hardcoded to maximize portability (they dont change anyway.)
	static LocData locations[NUMBER_OF_LOCATIONS];

	locations[10] = loadLoc("aboutEq");
	locations[8] = loadLoc("armsEq");
	locations[3] = loadLoc("bodyEq");
	locations[6] = loadLoc("feetEq");
	locations[1] = loadLoc("fingerOneEq");
	if (TWO_FILES)
	{
		locations[2] = loadLoc("fingerTwoEq");
	}
	else
	{
		locations[2] = loadLoc("fingerOneEq");
	}	
	locations[7] = loadLoc("handsEq");
	locations[4] = loadLoc("headEq");
	locations[15] = loadLoc("heldEq");
	locations[5] = loadLoc("legsEq");
	locations[0] = loadLoc("lightEq");
	locations[9] = loadLoc("shieldEq");
	locations[11] = loadLoc("waistEq");
	locations[14] = loadLoc("wieldEq");
	locations[12] = loadLoc("wristOneEq");
	if (TWO_FILES)
	{
		locations[13] = loadLoc("wristTwoEq");
	}
	else
	{
		locations[13] = loadLoc("wristOneEq");
	}	

	
	return locations;
	
}//end getAllEq	


//pre: none
//post: returns the LocData object containing all the eq from the specified location file 
LocData loadLoc(std::string location)
{
	//Initialize input stream
	
	char fileName[40];
	
	sprintf(fileName, "%s%s", location.c_str(), ".txt");

	std::ifstream input (fileName);

	LocData currLoc(location);
	EqData *nextNode;
	
	if (!input.is_open())
	{
		std::cout << "Error:: Couldn't initialize input stream for " << fileName << std::endl;
		exit(0);
	}

	std::string nextLine;

	while (!input.eof())
	{
		
		int hp = 0, mana = 0, hr = 0, dr = 0, ss = 0;
		bool dh = false, qo = false;
		std::string name = "";

	    //Load the piece of eq
		for (int stage = 0; stage < 7 ; stage++)
		{
			
			std::getline(input, nextLine);

			//Accomdate blank lines
			if (nextLine.compare("") != 0)
			{
				switch(stage)
				{
					//next
					case 0:
						if (toLower(nextLine).compare("next") == 0)
						{
							//Good. Not sure why this is here.
						}
						//Else formatting erorr
						else
						{
							std::cout << "Error:: found " << nextLine << " instead of next in " << fileName << std::endl;
							exit(0); 
						}	
						break;
					//hp
					case 1:
						hp = atoi(nextLine.c_str());
						break;
					//mana
					case 2:
				 		mana = atoi(nextLine.c_str());
						break;
					//hr
					case 3:
				    	hr = atoi(nextLine.c_str());
						break;
					//dr
					case 4:
				 		dr = atoi(nextLine.c_str());
						break;
					//ss
					case 5:
						ss = atoi(nextLine.c_str());
						break;
					//name
					case 6:
						name = nextLine;
						//Check for DH and QO
						if (toLower(name.substr(0,2)).compare("dh") == 0)
						{
							dh = true;
						}
						if (toLower(name.substr(0,2)).compare("qo") == 0)
						{
							qo = true;
						}		
						break;   	   
					default:
						std::cout << "Error:: reached default case in loadLoc switch. Terminating program." << std::endl;
						exit(0);
					break;
				
				}//End stage switch
			
			 
		    }//End not a blank line if
			//Else it was a blank line
			else
			{
				stage--;
				if (input.eof())
				{
					return currLoc;
				}
			}

		}//End eq for

		//At this point, all values should be initialized
		//if the location pointer hasn't been setup yet
		if (currLoc.getNext() == 0)
		{
			nextNode = new EqData(hp, mana, hr, dr, ss, name, dh, qo);
			currLoc.setNext(nextNode); 
			currLoc.setAmt(currLoc.getAmt() + 1);
		}
		else
		{
			EqData *temp = new EqData(hp, mana, hr, dr, ss, name, dh, qo);
			nextNode->setNext(temp);
			nextNode = temp;
			nextNode->setNext(NULL);
			currLoc.setAmt(currLoc.getAmt() + 1);
		}	

	}//End master eq while

	input.close();

	return currLoc;
	
}//end loadLoc	

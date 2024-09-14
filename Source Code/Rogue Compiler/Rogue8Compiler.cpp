//-----------------------------------------------------------
// Mujeeb Adelekan
// Rogue8 Compiler
// Rogue8Compiler.cpp
//-----------------------------------------------------------

#include <iostream>
#include <iomanip>

#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <vector>
#include <unordered_map> // Use for enum types. E.g., unordered_map<string, vector<string>> enumMap
                         //   where the key is the enum type name and the values are the constants for each type
                         //   This way, the index of each constant in the vector is that constant's corrsponding
                         //   integer value.

using namespace std;

//#define TRACEREADER
//#define TRACESCANNER
//#define TRACEPARSER
#define TRACEIDENTIFIERTABLE
#define TRACECOMPILER

//#define TESTENUMMAP     // uncomment to test the enumMap in the compiler

#include "Rogue.h"



/*
========================
Changes to Rogue7 compiler
========================
Added tokens
   (pseudo-terminals)
   (  reserved words) CHAR FLOAT PTR ADDR CONT FILE
                      ASSOC AS
                      ENUM
                      F_PRINT F_INPUT
                      F_CREATE
                      F_READ  F_WRITE
                      F_CLEAR
                      F_OPEN
                      F_CLOSE
                      F_DELETE
   (     punctuation) L_BRACKET R_BRACKET 
                      L_BRACE   R_BRACE
   (       operators) 

Updated functions
    ParseDataDefinitions
    ParseAssignmentStatement
    ParsePrimary
    ParseVariable
    ParseStatement
      
Added functions
    ParseAssociativeArrayReference
    ParseEnumDeclaration
    ParseFilePrintStatement
    ParseFileInputStatement
    ParseFileOperationStatement
*/

//-----------------------------------------------------------
typedef enum
//-----------------------------------------------------------
{
// pseudo-terminals
    IDENTIFIER,
    STRING,
    EOPTOKEN,
    UNKTOKEN,
    INTEGER,
    CHARACTER,  // added
    FLOATVAL,   // added
// reserved words
    MAIN,
    ENDMAIN,
    PRINT,
    OR,
    NOR,
    XOR,
    AND,
    NAND,
    NOT,
    TRUE,
    FALSE,
    INT, 
    BOOL,
    CHAR,  // ADDED
    FLOAT, // ADDED
    PTR,   // ADDED
    ADDR,  // ADDED
    CONT,  // ADDED
    FILE_T, // ADDED
    ASSOC,  // ADDED
    AS,     // ADDED
    ENUM,   // ADDED
    F_PRINT,  // ADDED 
    F_INPUT,  // ADDED
    F_CREATE, // ADDED
    F_READ,   // ADDED
    F_WRITE,  // ADDED
    F_CLEAR,  // ADDED
    F_OPEN,   // ADDED
    F_CLOSE,  // ADDED
    F_DELETE, // ADDED
    PERM, 
    INPUT,
    IF, 
    ELIF,
    ELSE, 
    DO, 
    WHILE,
    ENDIF, 
    ENDWHILE,
    ASSERT, 
    FOR, 
    TO, 
    BY, 
    ENDFOR,
    PROC, 
    ENDPROC, 
    IN, 
    OUT, 
    IO, 
    REF, 
    CALL, 
    RETURN,
    FUNC,
    ENDFUNC,
// punctuation
    COMMA,
    SEMICOLON,
    COLON,
    L_PAREN, //left parenthesis
    R_PAREN, //right parenthesis
    L_BRACKET, // left bracket
    R_BRACKET, // right bracket
    L_BRACE,
    R_BRACE,
// operators
    LT,
    LTEQ,
    EQ,
    GT,
    GTEQ,
    NOTEQ, // <> and !=
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    MODULUS,
    ABS,
    POWER, // ^ and **  
    ASSIGN,
    INC, 
    DEC
} TOKENTYPE;


//-----------------------------------------------------------
struct TOKENTABLERECORD
//-----------------------------------------------------------
{
   TOKENTYPE type;
   char description[12+1];
   bool isReservedWord;
};

//-----------------------------------------------------------
const TOKENTABLERECORD TOKENTABLE[] =
//-----------------------------------------------------------
{
// pseudo-terminals
    { IDENTIFIER  ,"IDENTIFIER"   ,false },
    { STRING      ,"STRING"       ,false },
    { EOPTOKEN    ,"EOPTOKEN"     ,false },
    { UNKTOKEN    ,"UNKTOKEN"     ,false },
    { INTEGER     ,"INTEGER"      ,false },
    { CHARACTER   ,"CHARACTER"    ,false },
    { FLOATVAL    ,"FLOATVAL"     ,false },
// reserved-words
    { MAIN        ,"MAIN"         ,true  },
    { ENDMAIN     ,"END MAIN"     ,true  },
    { PRINT       ,"PRINT"        ,true  },
    { OR          ,"OR"           ,true  },
    { NOR         ,"NOR"          ,true  },
    { XOR         ,"XOR"          ,true  },
    { AND         ,"AND"          ,true  },
    { NAND        ,"NAND"         ,true  },
    { NOT         ,"NOT"          ,true  },
    { TRUE        ,"TRUE"         ,true  },
    { FALSE       ,"FALSE"        ,true  },
    { INT         ,"INT"          ,true  },
    { BOOL        ,"BOOL"         ,true  },
    { CHAR        ,"CHAR"         ,true  }, // added
    { FLOAT       ,"FLOAT"        ,true  }, // added
    { PTR         ,"PTR"          ,true  }, // added
    { ADDR        ,"ADDR"         ,true  }, // added
    { CONT        ,"CONT"         ,true  }, // added
    { FILE_T      ,"FILE"         ,true  }, // added
    { ASSOC       ,"ASSOC"        ,true  }, // added
    { AS          ,"AS"           ,true  }, // added
    { ENUM        ,"ENUM"         ,true  }, // added
    { F_PRINT     ,"F_PRINT"      ,true  }, // added
    { F_INPUT     ,"F_INPUT"      ,true  }, // added
    { F_CREATE    ,"F_CREATE"     ,true  }, // added
    { F_READ      ,"F_READ"       ,true  }, // added
    { F_WRITE     ,"F_WRITE"      ,true  }, // added
    { F_CLEAR     ,"F_CLEAR"      ,true  }, // added
    { F_OPEN      ,"F_OPEN"       ,true  }, // added
    { F_CLOSE     ,"F_CLOSE"      ,true  }, // added
    { F_DELETE    ,"F_DELETE"     ,true  }, // added
    { PERM        ,"PERM"         ,true  },
    { INPUT       ,"INPUT"        ,true  },
    { IF          ,"IF"           ,true  },
    { ELSE        ,"ELSE"         ,true  },
    { ELIF        ,"ELIF"         ,true  },
    { DO          ,"DO"           ,true  },
    { WHILE       ,"WHILE"        ,true  },
    { ENDIF       ,"END IF"       ,true  },
    { ENDWHILE    ,"END WHILE"    ,true  },
    { ASSERT      ,"ASSERT"       ,true  },
    { FOR         ,"FOR"          ,true  },
    { TO          ,"TO"           ,true  },
    { BY          ,"BY"           ,true  },
    { ENDFOR      ,"END FOR"      ,true  },
    { PROC        ,"PROC"         ,true  },
    { ENDPROC     ,"END PROC"     ,true  },
    { IN          ,"IN"           ,true  },
    { OUT         ,"OUT"          ,true  },
    { IO          ,"IO"           ,true  },
    { REF         ,"REF"          ,true  },
    { CALL        ,"CALL"         ,true  },
    { RETURN      ,"RETURN"       ,true  },
    { FUNC        ,"FUNC"         ,true  },
    { ENDFUNC     ,"END FUNC"     ,true  },
// punctuation
    { COMMA       ,"COMMA"        ,false },
    { SEMICOLON   ,"SEMICOLON"    ,false },
    { L_PAREN     ,"LEFT PAREN"   ,false },
    { R_PAREN     ,"RIGHT PAREN"  ,false },
    { L_BRACKET   ,"LEFT BRACK"   ,false }, // added
    { R_BRACKET   ,"RIGHT BRACK"  ,false }, // added
    { L_BRACE     ,"LEFT BRACE"   ,false }, // added 
    { R_BRACE     ,"RIGHT BRACE"  ,false }, // added
// operators
    { LT          ,"LT"           ,false },
    { LTEQ        ,"LTEQ"         ,false },
    { EQ          ,"EQ"           ,false },
    { GT          ,"GT"           ,false },
    { GTEQ        ,"GTEQ"         ,false },
    { NOTEQ       ,"NOTEQ"        ,false },
    { PLUS        ,"PLUS"         ,false },
    { MINUS       ,"MINUS"        ,false },
    { MULTIPLY    ,"MULTIPLY"     ,false },
    { DIVIDE      ,"DIVIDE"       ,false },
    { MODULUS     ,"MODULUS"      ,false },
    { ABS         ,"ABS"          ,true  },
    { POWER       ,"POWER"        ,false },
    { ASSIGN      ,"ASSIGN"       ,false },
    { INC         ,"INC"          ,false },
    { DEC         ,"DEC"          ,false }
};

//-----------------------------------------------------------
struct TOKEN
//-----------------------------------------------------------
{
   TOKENTYPE type;
   char lexeme[SOURCELINELENGTH+1];
   int sourceLineNumber;
   int sourceLineIndex;
};

//--------------------------------------------------
// Global variables
//--------------------------------------------------
READER<CALLBACKSUSED> reader(SOURCELINELENGTH,LOOKAHEAD);
LISTER lister(LINESPERPAGE);
CODE code;
IDENTIFIERTABLE identifierTable(&lister,MAXIMUMIDENTIFIERS);

/*** ptr_indexes ***/
vector<int> ptr_indexes; // vector of pointer indexes

/*** enumMap ***/
// map of scoped enum types to their constant values
//     key type: IDENTIFIERSCOPE
//   value type: unordered_map< string, vector<string> >
// IDENTIFIERSCOPE and enumType are used as first and second keys,
//    respectively.
unordered_map< IDENTIFIERSCOPE, unordered_map<string, vector<string>> > enumMap;

/*** lValueEnumScope and rlalueEnumType ***/
// used for searching the enumMap for an r-value enum constant
IDENTIFIERSCOPE lValueEnumScope;     // stores the current scope of the l-value enum variable
string lValueEnumType = "";          //stores the enum type of the l-value enum variable


#ifdef TRACEPARSER
int level;
#endif

//-----------------------------------------------------------
void EnterModule(const char module[])
//-----------------------------------------------------------
{
#ifdef TRACEPARSER
   char information[SOURCELINELENGTH+1];

   level++;
   sprintf(information,"   %*s>%s",level*2," ",module);
   lister.ListInformationLine(information);
#endif
}

//-----------------------------------------------------------
void ExitModule(const char module[])
//-----------------------------------------------------------
{
#ifdef TRACEPARSER
   char information[SOURCELINELENGTH+1];

   sprintf(information,"   %*s<%s",level*2," ",module);
   lister.ListInformationLine(information);
   level--;
#endif
}

//--------------------------------------------------
void ProcessCompilerError(int sourceLineNumber, int sourceLineIndex, const char errorMessage[])
//--------------------------------------------------
{
    char information[SOURCELINELENGTH + 1];

// Use "panic mode" error recovery technique: report error message and terminate compilation!
sprintf(information, "     At (%4d:%3d) %s", sourceLineNumber, sourceLineIndex, errorMessage);
lister.ListInformationLine(information);
lister.ListInformationLine("Rogue compiler ending with compiler error!\n");
throw(ROGUEEXCEPTION("Rogue compiler ending with compiler error!"));
}

//-----------------------------------------------------------
int main()
//-----------------------------------------------------------
{
    void Callback1(int sourceLineNumber, const char sourceLine[]);
    void Callback2(int sourceLineNumber, const char sourceLine[]);
    void GetNextToken(TOKEN tokens[]);
    void ParseRogueProgram(TOKEN tokens[]);

    char sourceFileName[80 + 1];
    TOKEN tokens[LOOKAHEAD + 1];

    cout << "Source filename? ";
    cin >> sourceFileName;

    try
    {
        lister.OpenFile(sourceFileName);
        code.OpenFile(sourceFileName);

// CODEGENERATION
        code.EmitBeginningCode(sourceFileName);
// ENDCODEGENERATION

        reader.SetLister(&lister);
        reader.AddCallbackFunction(Callback1);
        reader.AddCallbackFunction(Callback2);
        reader.OpenFile(sourceFileName);

        // Fill tokens[] for look-ahead
        for (int i = 0; i <= LOOKAHEAD; i++)
            GetNextToken(tokens);

#ifdef TRACEPARSER
        level = 0;
#endif

        ParseRogueProgram(tokens);

// CODEGENERATION
        code.EmitEndingCode();
// ENDCODEGENERATION
    }
    catch (ROGUEEXCEPTION rogueException)
    {
        cout << "Rogue exception: " << rogueException.GetDescription() << endl;
    }
    lister.ListInformationLine("******* Rogue compiler ending");
    cout << "Rogue compiler ending\n";

    system("PAUSE");
    return(0);

}

//-----------------------------------------------------------
void ParseRogueProgram(TOKEN tokens[]) 
//-----------------------------------------------------------
{
    void ParseDataDefinitions(TOKEN tokens[], IDENTIFIERSCOPE identifierScope);
    void GetNextToken(TOKEN tokens[]);
    void ParseEnumDeclaration(TOKEN tokens[], IDENTIFIERSCOPE identifierScope);
    void ParseProcedureDefinition(TOKEN tokens[]);
    void ParseFunctionDefinition(TOKEN tokens[]);
    void ParseMainProgram(TOKEN tokens[]);

    EnterModule("RogueProgram");

 
    while ((tokens[0].type == ENUM))
    {
        ParseEnumDeclaration(tokens, GLOBALSCOPE);
    }
 
    ParseDataDefinitions(tokens, GLOBALSCOPE);

#ifdef TRACECOMPILER
    identifierTable.DisplayTableContents("Contents of identifier table after compilation of global data definitions");
#endif

    while ((tokens[0].type == PROC) || (tokens[0].type == FUNC) )
    {
        switch (tokens[0].type)
        {
            case PROC:
                ParseProcedureDefinition(tokens);
                break;
            case FUNC:
                ParseFunctionDefinition(tokens);
                break;
        }
    }



    if (tokens[0].type == MAIN)
        ParseMainProgram(tokens);
    else
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting MAIN");

    if (tokens[0].type != EOPTOKEN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting end-of-program");

    ExitModule("RogueProgram");
}
//-----------------------------------------------------------
void ParseDataDefinitions(TOKEN tokens[], IDENTIFIERSCOPE identifierScope) /*** updated ***/
//-----------------------------------------------------------
{
    void GetNextToken(TOKEN tokens[]);

    EnterModule("DataDefinitions");

    while ( (tokens[0].type == INT) || (tokens[0].type == BOOL) || (tokens[0].type == PERM)
            || (tokens[0].type == CHAR) || (tokens[0].type == FLOAT) || (tokens[0].type == PTR)
            || (tokens[0].type == FILE_T) || (tokens[0].type == ASSOC ) 
            || (enumMap[GLOBALSCOPE].find(tokens[0].lexeme) != enumMap[GLOBALSCOPE].end())            // check if the token is a global enumType
            || (enumMap[identifierScope].find(tokens[0].lexeme) != enumMap[identifierScope].end()))   // check if the token is a local enumType
    {
// <constantDefintions>
        if (tokens[0].type == PERM)
        {
            /* 
            Set varSeries to true if a datatype is followed by a comma-separated list of constants. 
               e.g.,      perm int w = 2, x = 4, bool y = true, z = false; 

            The above statement defines two integer constants w == 2 and x == 4
            and two boolean constants y == true and z == false.
            */
            bool varSeries = false;
            DATATYPE datatype;
            do
            {
                char identifier[MAXIMUMLENGTHIDENTIFIER + 1];
                char literal[MAXIMUMLENGTHIDENTIFIER + 1];
                char reference[MAXIMUMLENGTHIDENTIFIER + 1];
                char operand[MAXIMUMLENGTHIDENTIFIER + 1];
                char comment[MAXIMUMLENGTHIDENTIFIER + 1];
                bool isInTable;
                int index;

                GetNextToken(tokens);

                // Check if the token is <datatype>
                switch (tokens[0].type)
                {
                    case INT:
                        datatype = INTEGERTYPE;
                        if (!varSeries)
                            varSeries = true;
                        GetNextToken(tokens);
                        break;
                    case BOOL:
                        datatype = BOOLEANTYPE;
                        if (!varSeries)
                            varSeries = true;
                        GetNextToken(tokens);
                        break;
                    case CHAR:
                        datatype = CHARACTERTYPE;
                        if (!varSeries)
                            varSeries = true;
                        GetNextToken(tokens);
                        break;
                    case FLOAT:
                        datatype = FLOATTYPE;
                        if (!varSeries)
                            varSeries = true;
                        GetNextToken(tokens);
                        break;
                    case IDENTIFIER:
                        if (!varSeries)
                            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting 'int', 'bool', 'char', or 'float'");
                        break;
                    default:
                        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting 'int', 'bool', 'char', or 'float'");
                        
                }

                // Check if token is <identifier>
                if (tokens[0].type != IDENTIFIER)
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");
                strcpy(identifier, tokens[0].lexeme);
                GetNextToken(tokens);

                

                // Check if token is the assign operator '='
                if (tokens[0].type != ASSIGN)
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '='");
                GetNextToken(tokens);

                // Analyze the data type of the <literal>
                if ((datatype == INTEGERTYPE) && (tokens[0].type == INTEGER))
                {
                    strcpy(literal, "0D");
                    strcat(literal, tokens[0].lexeme);
                }
                else if (((datatype == BOOLEANTYPE) && (tokens[0].type == TRUE))
                    || ((datatype == BOOLEANTYPE) && (tokens[0].type == FALSE)))
                {
                    strcpy(literal, tokens[0].lexeme);
                }
                else if ((datatype == CHARACTERTYPE) && (tokens[0].type == CHARACTER))
                {
                    strcpy(literal, tokens[0].lexeme);
                }
                else if ((datatype == FLOATTYPE) && (tokens[0].type == FLOATVAL))
                {
                    strcpy(literal, "0F");
                    strcat(literal, tokens[0].lexeme);
                }
                else
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Data type mismatch");
                GetNextToken(tokens);

                index = identifierTable.GetIndex(identifier, isInTable);
                if (isInTable && identifierTable.IsInCurrentScope(index))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Multiply-defined identifier");

                switch (identifierScope)
                {
                    case GLOBALSCOPE:
// CODEGENERATION
                        code.AddDWToStaticData(literal, identifier, reference);
// ENDCODEGENERATION
                        identifierTable.AddToTable(identifier, GLOBAL_CONSTANT, datatype, reference);
                        break;
                    case PROGRAMMODULESCOPE:
// CODEGENERATION
                        code.AddDWToStaticData(literal, identifier, reference);
// ENDCODEGENERATION
                        identifierTable.AddToTable(identifier, PROGRAMMODULE_CONSTANT, datatype, reference);
                        break;
                    case SUBPROGRAMMODULESCOPE:
// CODEGENERATION
                        sprintf(reference, "FB:0D%d", code.GetFBOffset());
                        strcpy(operand, "#"); strcat(operand, literal);
                        sprintf(comment, "initialize constant %s", identifier);
                        code.AddInstructionToInitializeFrameData("PUSH", operand, comment);
                        code.AddInstructionToInitializeFrameData("POP", reference);
                        code.IncrementFBOffset(1);
// ENDCODEGENERATION
                        identifierTable.AddToTable(identifier, SUBPROGRAMMODULE_CONSTANT, datatype, reference);
                        break;
    
                }

            } while ( tokens[0].type == COMMA );

            if (tokens[0].type != SEMICOLON)
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");
            GetNextToken(tokens);
        }

//  <AssociativeArrayDefinition>
        else if (tokens[0].type == ASSOC)
        {
        char identifier[MAXIMUMLENGTHIDENTIFIER + 1];
        char operand[MAXIMUMLENGTHIDENTIFIER + 1];
        char comment[MAXIMUMLENGTHIDENTIFIER + 1];
        char reference[MAXIMUMLENGTHIDENTIFIER + 1];
        bool isInTable;
        int index;
        int capacity; // the capacity of the array

        GetNextToken(tokens);

        // Check if token is <identifer>
        if (tokens[0].type != IDENTIFIER)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");
        strcpy(identifier, tokens[0].lexeme);
        GetNextToken(tokens);

        index = identifierTable.GetIndex(identifier, isInTable);
        if (isInTable && identifierTable.IsInCurrentScope(index))
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Multiply-defined identifier");

        // Check for a left brace: '{'
        if (tokens[0].type != L_BRACE)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '{'");
        GetNextToken(tokens);

        // Check if the associative array capacity is an integer literal
        if (tokens[0].type != INTEGER)
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Associative array capacity must be an integer literal");
        }

        // Assign the capacity with the integer value
        capacity = atoi(tokens[0].lexeme);

        GetNextToken(tokens);

        // Check for a right brace: '}'
        if (tokens[0].type != R_BRACE)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '}'");
        GetNextToken(tokens);

        if (tokens[0].type != SEMICOLON)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");
        GetNextToken(tokens);

// CODEGENERATION
        int base; // used for the SUBPROGRAMMODULESCOPE

        switch (identifierScope)
        {
        case GLOBALSCOPE:
            // Reserve a word in STATICDATA for the size of the associative array (initial value is 0)
            sprintf(comment, "%s at SB:0D%d", identifier, code.GetSBOffset());
            code.AddRWToStaticData(1, comment, reference);

            // Add to the identifier table as a one-dimensional associative array
            identifierTable.AddToTable(identifier, GLOBAL_VARIABLE, ASSOCTYPE, reference, 1);

            // Define a word in STATICDATA with the capacity of the associative array
            sprintf(operand, "0D%d", capacity);
            code.AddDWToStaticData(operand, "", reference);

            // Reserve words for each (key, value) pair in the associative array
            code.AddRWToStaticData(2 * capacity, "", reference);

           

            break;
        case PROGRAMMODULESCOPE:
            // Reserve a word in STATICDATA for the size of the associative array (initial value is 0)
            sprintf(comment, "%s at SB:0D%d", identifier, code.GetSBOffset());
            code.AddRWToStaticData(1, comment, reference);

            // Add to the identifier table as a one-dimensional associative array
            identifierTable.AddToTable(identifier, PROGRAMMODULE_VARIABLE, ASSOCTYPE, reference, 1);

            // Define a word in STATICDATA with the capacity of the associative array
            sprintf(operand, "0D%d", capacity);
            code.AddDWToStaticData(operand, "", reference);

            // Reserve words for each (key, value) pair in the associative array
            code.AddRWToStaticData(2 * capacity, "", reference);

            
            break;
        case SUBPROGRAMMODULESCOPE:

            code.IncrementFBOffset(1 + 2 * capacity); // not 2+2*capacity because 1 word
            base = code.GetFBOffset();                // is already available because of the 
            code.IncrementFBOffset(1);                // "FBOffset points-to next available word" rule
            sprintf(reference, "FB:0D%d", base);
            identifierTable.AddToTable(identifier, SUBPROGRAMMODULE_VARIABLE, ASSOCTYPE, reference, 1);

            // Add the initial value for the size, 0, to the activation record
            sprintf(reference, "FB:0D%d", base);
            sprintf(operand, "#0D0");
            sprintf(comment, "initialize associative array %s at FB:0D%d", identifier, base);
            code.AddInstructionToInitializeFrameData("PUSH", operand, comment);
            code.AddInstructionToInitializeFrameData("POP", reference);

            // Add the capacity to the activation record
            sprintf(reference, "FB:0D%d", base - 1);
            sprintf(operand, "#0D%d", capacity);
            code.AddInstructionToInitializeFrameData("PUSH", operand);
            code.AddInstructionToInitializeFrameData("POP", reference);

            break;
        }
// ENDCODEGENERATION


        }

//  <variableDefinitions>
        else
        {
            /*
            Set varSeries to true if a datatype is followed by a comma-separated list of variables.
               e.g.,      int w, x, bool y, z;

            The above statement defines two integer variables w and x
            and two boolean variables y and z.
            */
            bool varSeries = false;
            DATATYPE datatype;
            bool firstLoop = true;

             
            do
            {
                char identifier[MAXIMUMLENGTHIDENTIFIER + 1];
                char operand[MAXIMUMLENGTHIDENTIFIER + 1];
                char comment[MAXIMUMLENGTHIDENTIFIER + 1];
                char reference[MAXIMUMLENGTHIDENTIFIER + 1];
                bool isInTable;
                int index;
                int dimensions; // number of dimensions in array (0 is scalar variable)
                vector<int> capacities; // vector of the n capacities in an n-dimensional array
                int totalCapacity; // the total capacity of the array
               
                char enumType[MAXIMUMLENGTHIDENTIFIER + 1];  // the enumType for enumerated variables
                
                if (firstLoop)
                {
                    firstLoop = false;
                }
                else
                {
                    GetNextToken(tokens);
                }

                // Check if the token is <datatype>
                switch (tokens[0].type)
                {
                case INT:
                    datatype = INTEGERTYPE;
                    if (!varSeries)
                        varSeries = true;
                    GetNextToken(tokens);
                    break;
                case BOOL:
                    datatype = BOOLEANTYPE;
                    if (!varSeries)
                        varSeries = true;
                    GetNextToken(tokens);
                    break;
                case CHAR:
                    datatype = CHARACTERTYPE;
                    if (!varSeries)
                        varSeries = true;
                    GetNextToken(tokens);
                    break;
                case FLOAT:
                    datatype = FLOATTYPE;
                    if (!varSeries)
                        varSeries = true;
                    GetNextToken(tokens);
                    break;
                case PTR:
                    datatype = POINTERTYPE;
                    if (!varSeries)
                        varSeries = true;
                    GetNextToken(tokens);
                    break;
                case FILE_T:
                    datatype = FILETYPE;
                    if (!varSeries)
                        varSeries = true;
                    GetNextToken(tokens);
                    break;
                case IDENTIFIER:

                    // Get the index of the identifier to check its datatype.
                    index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);

                    if (!varSeries)
                    {
                        
                        // If there isn't a variable series AND <identifier> is NOT an <enumType>:
                        if (identifierTable.GetType(index) != GLOBAL_ENUMTYPE &&
                            identifierTable.GetType(index) != PROGRAMMODULE_ENUMTYPE &&
                            identifierTable.GetType(index) != SUBPROGRAMMODULE_ENUMTYPE)
                        {
                            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, 
                                "Expecting 'int', 'bool', 'char', 'float', 'ptr', 'file', or an enum type");
                        }

                        // If there isn't a variable series BUT <identifier> IS an <enumType>:
                        datatype = ENUMTYPE;
                        strcpy(enumType, tokens[0].lexeme);
                        varSeries = true;
                        GetNextToken(tokens);
                        
                      
                    }
                    else 
                    {
                        // If there IS a variable series AND <identifier> IS an <enumType>:
                        if (identifierTable.GetType(index) == GLOBAL_ENUMTYPE ||
                            identifierTable.GetType(index) == PROGRAMMODULE_ENUMTYPE ||
                            identifierTable.GetType(index) == SUBPROGRAMMODULE_ENUMTYPE)
                        {

                            datatype = ENUMTYPE;
                            strcpy(enumType, tokens[0].lexeme);
                            GetNextToken(tokens);
                        }
                        
                    }

                    break;
                
                default:
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, 
                        "Expecting 'int', 'bool', 'char', 'float', 'ptr', 'file', or an enum type");
                }
                
                // Check if token is <identifier>
                if (tokens[0].type != IDENTIFIER)
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");
                strcpy(identifier, tokens[0].lexeme);
                GetNextToken(tokens);

                index = identifierTable.GetIndex(identifier, isInTable);
                if (isInTable && identifierTable.IsInCurrentScope(index))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Multiply-defined identifier");

                // Set dimensions to 0 by default
                dimensions = 0;


                // Check for a left bracket: '['
                if (tokens[0].type == L_BRACKET)
                {
                    // Clear the vector of n capacities
                    capacities.clear();

                    // Initalize the total capacity to 1
                    totalCapacity = 1;

                    // Parse the comma separated list of capacities
                    do
                    {
                        // Get the next token
                        GetNextToken(tokens);

                        // Check if the array capacity is an integer literal
                        if (tokens[0].type != INTEGER)
                        {
                            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                                "Array capacity must be an integer literal");
                        }
                        

                        // Push the capacity into array
                        capacities.push_back(atoi(tokens[0].lexeme));

                        // Increment dimensions
                        dimensions++;

                        // update the total capatity of the array
                        totalCapacity *= capacities.back();

                        // Get next token
                        GetNextToken(tokens);

                    } while (tokens[0].type == COMMA);

                    // Check for a right bracket: ']'
                    if (tokens[0].type != R_BRACKET)
                    {
                        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                            "Expecting ']'");
                    }
                    GetNextToken(tokens);

                }

                // If the variable is a scalar
                if (dimensions == 0)
                {
                    switch (identifierScope)
                    {
                    case GLOBALSCOPE:
                        // CODEGENERATION
                        code.AddRWToStaticData(1, identifier, reference);
                        // ENDCODEGENERATION

                        identifierTable.AddToTable(identifier, GLOBAL_VARIABLE, datatype, reference);
                        break;
                    case PROGRAMMODULESCOPE:
                        // CODEGENERATION
                        code.AddRWToStaticData(1, identifier, reference);
                        // ENDCODEGENERATION

                        identifierTable.AddToTable(identifier, PROGRAMMODULE_VARIABLE, datatype, reference);

                        break;
                    case SUBPROGRAMMODULESCOPE:
                        // CODEGENERATION
                        sprintf(reference, "FB:0D%d", code.GetFBOffset());
                        code.IncrementFBOffset(1);
                        // ENDCODEGENERATION

                        identifierTable.AddToTable(identifier, SUBPROGRAMMODULE_VARIABLE, datatype, reference);
                        break;
                    }

                    if (datatype == ENUMTYPE)
                    {
                        // save the enumerated type for the variable
                        index = identifierTable.GetIndex(identifier, isInTable);
                        identifierTable.SetEnumType(index, enumType);
                    }    

                }
                // If the variable is an array
                else
                {
                    // CODEGENERATION
                    int base;

                    switch (identifierScope)
                    {
                    case GLOBALSCOPE:
                        sprintf(operand, "0D%d", dimensions);
                        sprintf(comment, "%s at SB:0D%d", identifier, code.GetSBOffset());
                        code.AddDWToStaticData(operand, comment, reference);
                        identifierTable.AddToTable(identifier, GLOBAL_VARIABLE, datatype, reference, dimensions);
                        for (int i = 1; i <= dimensions; i++)
                        {
                            sprintf(operand, "0D0");
                            code.AddDWToStaticData(operand, "", reference);
                            sprintf(operand, "0D%d", capacities[i - 1] - 1);
                            code.AddDWToStaticData(operand, "", reference);
                        }
                        code.AddRWToStaticData(totalCapacity, "", reference);
                        break;
                    case PROGRAMMODULESCOPE:
                        sprintf(operand, "0D%d", dimensions);
                        sprintf(comment, "%s at SB:0D%d", identifier, code.GetSBOffset());
                        code.AddDWToStaticData(operand, comment, reference);
                        identifierTable.AddToTable(identifier, PROGRAMMODULE_VARIABLE, datatype, reference, dimensions);
                        for (int i = 1; i <= dimensions; i++)
                        {
                            sprintf(operand, "0D0");
                            code.AddDWToStaticData(operand, "", reference);
                            sprintf(operand, "0D%d", capacities[i - 1] - 1);
                            code.AddDWToStaticData(operand, "", reference);
                        }
                        code.AddRWToStaticData(totalCapacity, "", reference);
                        break;
                    case SUBPROGRAMMODULESCOPE:
                        code.IncrementFBOffset(2 * dimensions + totalCapacity);    // not 1+2*dimensions+totalCapacity because 1 word 
                        base = code.GetFBOffset();                        // is already available because of 
                        code.IncrementFBOffset(1);                        // "FBOffset points-to next available word" rule
                        sprintf(reference, "FB:0D%d", base);
                        identifierTable.AddToTable(identifier, SUBPROGRAMMODULE_VARIABLE, datatype, reference, dimensions);

                        sprintf(reference, "FB:0D%d", base);
                        sprintf(operand, "#0D%d", dimensions);
                        sprintf(comment, "initialize array %s at FB:0D%d", identifier, base);
                        code.AddInstructionToInitializeFrameData("PUSH", operand, comment);
                        code.AddInstructionToInitializeFrameData("POP", reference);
                        for (int i = 1; i <= dimensions; i++)
                        {
                            sprintf(operand, "#0D0"); 
                            code.AddInstructionToInitializeFrameData("PUSH", operand);
                            sprintf(reference, "FB:0D%d", base - (2 * (i - 1) + 1));
                            code.AddInstructionToInitializeFrameData("POP", reference);

                            sprintf(operand, "#0D%d", capacities[i - 1] - 1);
                            code.AddInstructionToInitializeFrameData("PUSH", operand);
                            sprintf(reference, "FB:0D%d", base - (2 * (i - 1) + 2));
                            code.AddInstructionToInitializeFrameData("POP", reference);
                        }
                        break;
                    }

                    if (datatype == ENUMTYPE)
                    {
                        // save the enumerated type for the variable
                        index = identifierTable.GetIndex(identifier, isInTable);
                        identifierTable.SetEnumType(index, enumType);
                    }
                    
                }
                // ENDCODEGENERATION               

            } while (tokens[0].type == COMMA);

            if (tokens[0].type != SEMICOLON)
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");
            GetNextToken(tokens);

        }
    }
   ExitModule("DataDefinitions");

}


//-----------------------------------------------------------
void ParseEnumDeclaration(TOKEN tokens[], IDENTIFIERSCOPE identifierScope)  /***added***/
//-----------------------------------------------------------
{
    void GetNextToken(TOKEN stokens[]);

    char enumType[MAXIMUMLENGTHIDENTIFIER + 1];
    char enumConstant[MAXIMUMLENGTHIDENTIFIER + 1];
    int index;
    bool isInTable;

    EnterModule("EnumDeclaration");

    // Get the next token
    GetNextToken(tokens);

    // Check for colon
    if (tokens[0].type != COLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ':'");
    GetNextToken(tokens);

    // Check for <enumType> ::= <identifier>
    if (tokens[0].type != IDENTIFIER)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifer");

    strcpy(enumType, tokens[0].lexeme);
    index = identifierTable.GetIndex(enumType, isInTable);
    if (isInTable && identifierTable.IsInCurrentScope(index))
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Multiply-defined identifier");

    // Add to the identifier table as an enumeration type
    switch (identifierScope)
    {
    case GLOBALSCOPE:
        identifierTable.AddToTable(enumType, GLOBAL_ENUMTYPE, NOTYPE, "\0");
        break;
    case PROGRAMMODULESCOPE:
        identifierTable.AddToTable(enumType, PROGRAMMODULE_ENUMTYPE, NOTYPE, "\0");
        break;
    case SUBPROGRAMMODULESCOPE:
        identifierTable.AddToTable(enumType, SUBPROGRAMMODULE_ENUMTYPE, NOTYPE, "\0");
        break;
    }
    

    // Get the next token
    GetNextToken(tokens);

    // Check for left brace: '{'
    if (tokens[0].type != L_BRACE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '{'");

    // Scan all the enum constants
    do
    {
        // Get the next token
        GetNextToken(tokens);

        // Check for <identifier>
        if (tokens[0].type != IDENTIFIER)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifer");

        strcpy(enumConstant, tokens[0].lexeme);

        // Ensure that all values for enumType are unique
        for (string value : enumMap[identifierScope][enumType])
        {
            if (value == enumConstant)
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Duplicate enumeration constant");
        }

        // Store the enumConstant as a value for enumType
        enumMap[identifierScope][enumType].push_back(enumConstant);

        GetNextToken(tokens);

    } while (tokens[0].type == COMMA);

    // Check for right brace: '}'
    if (tokens[0].type != R_BRACE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '}'");
    GetNextToken(tokens);

    // Check for semicolon: ';'
    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");
    GetNextToken(tokens);

// *** TESTING that enumMap works *** (it works!)
#ifdef TESTENUMMAP
    for (auto entry : enumMap)
    {
        // Prints: 
        // '0' if GLOBALSCOPE
        // '1' if PROGRAMMODULESCOPE
        // '2' if SUBPROGRAMMODULESCOPE
        cout << entry.first << " | ";

        for (auto val : entry.second)
        {
            cout << val.first << " : ";

            for (string constant : val.second)
            {
                cout << constant << ", ";
            }

            cout << "|| ";

        }

        cout << endl;         
    }
#endif

    

    ExitModule("EnumDeclaration");

}


//-----------------------------------------------------------
void ParseProcedureDefinition(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseFormalParameter(TOKEN tokens[], IDENTIFIERTYPE & identifierType, int& n);
    void ParseEnumDeclaration(TOKEN tokens[], IDENTIFIERSCOPE identifierScope);
    void ParseDataDefinitions(TOKEN tokens[], IDENTIFIERSCOPE identifierScope);
    void ParseStatement(TOKEN tokens[]);
    void GetNextToken(TOKEN tokens[]);

    bool isInTable;
    char line[SOURCELINELENGTH + 1];
    int index;
    char reference[SOURCELINELENGTH + 1];

// n = # formal parameters, m = # words of "save-register" space and locally-defined variables/constants
    int n, m;
    char label[SOURCELINELENGTH + 1], operand[SOURCELINELENGTH + 1], comment[SOURCELINELENGTH + 1];

    EnterModule("ProcedureDefinition");

    GetNextToken(tokens);

    if (tokens[0].type != IDENTIFIER)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");

    index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
    if (isInTable && identifierTable.IsInCurrentScope(index))
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Multiply-defined identifier");

    identifierTable.AddToTable(tokens[0].lexeme, PROCEDURE_SUBPROGRAMMODULE, NOTYPE, tokens[0].lexeme);

// CODEGENERATION
    code.EnterModuleBody(PROCEDURE_SUBPROGRAMMODULE, index);
    code.ResetFrameData();
    code.EmitUnformattedLine("; **** =========");
    sprintf(line, "; **** PROCEDURE module (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    code.EmitUnformattedLine("; **** =========");
    code.EmitFormattedLine(tokens[0].lexeme, "EQU", "*");
// ENDCODEGENERATION

    identifierTable.EnterNestedStaticScope();

    GetNextToken(tokens);
    n = 0;
    if (tokens[0].type == L_PAREN)
    {
        do
        {
            IDENTIFIERTYPE identifierType;

            GetNextToken(tokens);
            ParseFormalParameter(tokens, identifierType, n);
        } while (tokens[0].type == COMMA);

        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
        GetNextToken(tokens);
    }

#ifdef TRACECOMPILER
    identifierTable.DisplayTableContents("Contents of identifier table after compilation of PROCEDURE module header");
#endif

// CODEGENERATION
    code.IncrementFBOffset(2); // makes room in frame for caller's saved FB register and the CALL return address
// ENDCODEGENERATION

    while (tokens[0].type == ENUM)
    {
        ParseEnumDeclaration(tokens, SUBPROGRAMMODULESCOPE);
    }

    ParseDataDefinitions(tokens, SUBPROGRAMMODULESCOPE);

#ifdef TRACECOMPILER
    identifierTable.DisplayTableContents("Contents of identifier table after compilation of PROCEDURE local data definitions");
#endif

// CODEGENERATION
    m = code.GetFBOffset() - (n + 2);
    code.EmitFormattedLine("", "PUSHSP", "", "set PROCEDURE module FB = SP-on-entry + 2(n+2)");
    sprintf(operand, "#0D%d", 2 * (n + 2));
    sprintf(comment, "n = %d", n);
    code.EmitFormattedLine("", "PUSH", operand, comment);
    code.EmitFormattedLine("", "ADDI");
    code.EmitFormattedLine("", "POPFB");
    code.EmitFormattedLine("", "PUSHSP", "", "PROCEDURE module SP = SP-on-entry + 2m");
    sprintf(operand, "#0D%d", 2 * m);
    sprintf(comment, "m = %d", m);
    code.EmitFormattedLine("", "PUSH", operand, comment);
    code.EmitFormattedLine("", "SUBI");
    code.EmitFormattedLine("", "POPSP");
    code.EmitUnformattedLine("; statements to initialize frame data (if necessary)");
    code.EmitFrameData();
    sprintf(label, "MODULEBODY%04d", code.LabelSuffix());
    code.EmitFormattedLine("", "CALL", label);
    code.EmitFormattedLine("", "PUSHFB", "", "restore caller's SP-on-entry = FB - 2(n+2)");
    sprintf(operand, "#0D%d", 2 * (n + 2));
    code.EmitFormattedLine("", "PUSH", operand);
    code.EmitFormattedLine("", "SUBI");
    code.EmitFormattedLine("", "POPSP");
    code.EmitFormattedLine("", "RETURN", "", "return to caller");
    code.EmitUnformattedLine("");
    code.EmitFormattedLine(label, "EQU", "*");
    code.EmitUnformattedLine("; statements in body of PROCEDURE module (may include RETURN)");
// ENDCODEGENERATION

    while (tokens[0].type != ENDPROC)
        ParseStatement(tokens);

// CODEGENERATION
    code.EmitFormattedLine("", "RETURN");
    code.EmitUnformattedLine("");
    code.EmitUnformattedLine("; **** =========");
    sprintf(line, "; **** END (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    code.EmitUnformattedLine("; **** =========");
    code.ExitModuleBody();
// ENDCODEGENERATION

    identifierTable.ExitNestedStaticScope();

    // Clear all subprogram-scoped enumeration types from enumMap
    enumMap.erase(SUBPROGRAMMODULESCOPE);

#ifdef TRACECOMPILER
    identifierTable.DisplayTableContents("Contents of identifier table at end of compilation of PROCEDURE module definition");
#endif

    GetNextToken(tokens);

    ExitModule("ProcedureDefinition");
}

//-----------------------------------------------------------
void ParseFunctionDefinition(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseFormalParameter(TOKEN tokens[], IDENTIFIERTYPE & identifierType, int& n);
    void ParseEnumDeclaration(TOKEN tokens[], IDENTIFIERSCOPE identifierScope);
    void ParseDataDefinitions(TOKEN tokens[], IDENTIFIERSCOPE identifierScope);
    void ParseStatement(TOKEN tokens[]);
    void GetNextToken(TOKEN tokens[]);

    bool isInTable;
    DATATYPE datatype;
    char identifier[SOURCELINELENGTH + 1];
    char line[SOURCELINELENGTH + 1];
    int index;
    char reference[SOURCELINELENGTH + 1];

 // n = # formal parameters, m = # words of return-value, "save-register" space, and locally-defined variables/constants
    int n, m;
    char label[SOURCELINELENGTH + 1], operand[SOURCELINELENGTH + 1], comment[SOURCELINELENGTH + 1];

    EnterModule("FunctionDefinition");

    GetNextToken(tokens);

    if (tokens[0].type != IDENTIFIER)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");

    strcpy(identifier, tokens[0].lexeme);
    index = identifierTable.GetIndex(identifier, isInTable);
    if (isInTable && identifierTable.IsInCurrentScope(index))
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Multiply-defined identifier");
    GetNextToken(tokens);

    if (tokens[0].type != COLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ':'");
    GetNextToken(tokens);

    switch (tokens[0].type)
    {
    case INT:
        datatype = INTEGERTYPE;
        break;
    case BOOL:
        datatype = BOOLEANTYPE;
        break;
    case CHAR:
        datatype = CHARACTERTYPE;
        break;
    case FLOAT:
        datatype = FLOATTYPE;
        break;
    default:
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting int, bool, char, or float");
    }
    GetNextToken(tokens);

    identifierTable.AddToTable(identifier, FUNCTION_SUBPROGRAMMODULE, datatype, identifier);
    index = identifierTable.GetIndex(identifier, isInTable);

// CODEGENERATION
    code.EnterModuleBody(FUNCTION_SUBPROGRAMMODULE, index);
    code.ResetFrameData();

    // Reserve frame-space for FUNCTION return value
    code.IncrementFBOffset(1);

    code.EmitUnformattedLine("; **** =========");
    sprintf(line, "; **** FUNCTION module (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    code.EmitUnformattedLine("; **** =========");
    code.EmitFormattedLine(identifier, "EQU", "*");
 // ENDCODEGENERATION

    identifierTable.EnterNestedStaticScope();

    n = 0;
    if (tokens[0].type != L_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '('");

    // Use token look-ahead to make parsing decision
    if (tokens[1].type != R_PAREN)
    {
        do
        {
            IDENTIFIERTYPE identifierType;

            GetNextToken(tokens);
            ParseFormalParameter(tokens, identifierType, n);

            // STATICSEMANTICS
            if (identifierType != IN_PARAMETER)
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Function parameter must be IN");
            // ENDSTATICSEMANTICS

        } while (tokens[0].type == COMMA);
    }
    else
        GetNextToken(tokens);
    if (tokens[0].type != R_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
    GetNextToken(tokens);

#ifdef TRACECOMPILER
        identifierTable.DisplayTableContents("Contents of identifier table after compilation of FUNCTION module header");
#endif

// CODEGENERATION
    code.IncrementFBOffset(2); // makes room in frame for caller's saved FB register and the CALL return address
 // ENDCODEGENERATION

    while (tokens[0].type == ENUM)
    {
        ParseEnumDeclaration(tokens, SUBPROGRAMMODULESCOPE);
    }

    ParseDataDefinitions(tokens, SUBPROGRAMMODULESCOPE);

#ifdef TRACECOMPILER
    identifierTable.DisplayTableContents("Contents of identifier table after compilation of FUNCTION local data definitions");
#endif

// CODEGENERATION
    m = code.GetFBOffset() - (n + 3);
    code.EmitFormattedLine("", "PUSHSP", "", "set FUNCTION module FB = SP-on-entry + 2(n+3)");
    sprintf(operand, "#0D%d", 2 * (n + 3));
    sprintf(comment, "n = %d", n);
    code.EmitFormattedLine("", "PUSH", operand, comment);
    code.EmitFormattedLine("", "ADDI");
    code.EmitFormattedLine("", "POPFB");
    code.EmitFormattedLine("", "PUSHSP", "", "FUNCTION module SP = SP-on-entry + 2m");
    sprintf(operand, "#0D%d", 2 * m);
    sprintf(comment, "m = %d", m);
    code.EmitFormattedLine("", "PUSH", operand, comment);
    code.EmitFormattedLine("", "SUBI");
    code.EmitFormattedLine("", "POPSP");
    code.EmitUnformattedLine("; statements to initialize frame data (if necessary)");
    code.EmitFrameData();
    sprintf(label, "MODULEBODY%04d", code.LabelSuffix());
    code.EmitFormattedLine("", "CALL", label);
    code.EmitFormattedLine("", "PUSHFB", "", "restore caller's SP-on-entry = FB - 2(n+3)");
    sprintf(operand, "#0D%d", 2 * (n + 3));
    code.EmitFormattedLine("", "PUSH", operand);
    code.EmitFormattedLine("", "SUBI");
    code.EmitFormattedLine("", "POPSP");
    code.EmitFormattedLine("", "RETURN", "", "return to caller");
    code.EmitUnformattedLine("");
    code.EmitFormattedLine(label, "EQU", "*");
    code.EmitUnformattedLine("; statements in body of FUNCTION module (*MUST* execute RETURN)");
// ENDCODEGENERATION

    while (tokens[0].type != ENDFUNC)
        ParseStatement(tokens);

// CODEGENERATION
    sprintf(operand, "#0D%d", tokens[0].sourceLineNumber);
    code.EmitFormattedLine("", "PUSH", operand);
    code.EmitFormattedLine("", "PUSH", "#0D3");
    code.EmitFormattedLine("", "JMP", "HANDLERUNTIMEERROR");
    code.EmitUnformattedLine("; **** =========");
    sprintf(line, "; **** END (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    code.EmitUnformattedLine("; **** =========");
    code.ExitModuleBody();
// ENDCODEGENERATION

    identifierTable.ExitNestedStaticScope();

    // Clear all subprogram-scoped enumeration types from enumMap
    enumMap.erase(SUBPROGRAMMODULESCOPE);

#ifdef TRACECOMPILER
    identifierTable.DisplayTableContents("Contents of identifier table at end of compilation of FUNCTION module definition");
#endif

    GetNextToken(tokens);

    ExitModule("FunctionDefinition");
}

//-----------------------------------------------------------
void ParseFormalParameter(TOKEN tokens[], IDENTIFIERTYPE& identifierType, int& n)
//-----------------------------------------------------------
{
    void GetNextToken(TOKEN tokens[]);

    char identifier[MAXIMUMLENGTHIDENTIFIER + 1], reference[MAXIMUMLENGTHIDENTIFIER + 1];
    bool isInTable;
    int index;
    DATATYPE datatype;

    EnterModule("FormalParameter");

    // CODEGENERATION
    switch (tokens[0].type)
    {
    case IN:
        identifierType = IN_PARAMETER;
        sprintf(reference, "FB:0D%d", code.GetFBOffset());
        code.IncrementFBOffset(1);
        n += 1;
        GetNextToken(tokens);
        break;
    case OUT:
        identifierType = OUT_PARAMETER;
        code.IncrementFBOffset(1);
        sprintf(reference, "FB:0D%d", code.GetFBOffset());
        code.IncrementFBOffset(1);
        n += 2;
        GetNextToken(tokens);
        break;
    case IO:
        identifierType = IO_PARAMETER;
        code.IncrementFBOffset(1);
        sprintf(reference, "FB:0D%d", code.GetFBOffset());
        code.IncrementFBOffset(1);
        n += 2;
        GetNextToken(tokens);
        break;
    case REF:
        identifierType = REF_PARAMETER;
        sprintf(reference, "@FB:0D%d", code.GetFBOffset());
        code.IncrementFBOffset(1);
        n += 1;
        GetNextToken(tokens);
        break;
    default:
        identifierType = IN_PARAMETER;
        sprintf(reference, "FB:0D%d", code.GetFBOffset());
        code.IncrementFBOffset(1);
        n += 1;
        break;
    }
    // ENDCODEGENERATION

    if (tokens[0].type != IDENTIFIER)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");
    strcpy(identifier, tokens[0].lexeme);
    GetNextToken(tokens);

    if (tokens[0].type != COLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ':'");
    GetNextToken(tokens);

    switch (tokens[0].type)
    {
        case INT:
            datatype = INTEGERTYPE;
            break;
        case BOOL:
            datatype = BOOLEANTYPE;
            break;
        case CHAR:
            datatype = CHARACTERTYPE;
            break;
        case FLOAT:
            datatype = FLOATTYPE;
            break;
        default:
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting int, bool, char, or float");
    }
    GetNextToken(tokens);

    index = identifierTable.GetIndex(identifier, isInTable);
    if (isInTable && identifierTable.IsInCurrentScope(index))
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Multiply-defined identifier");

    identifierTable.AddToTable(identifier, identifierType, datatype, reference);

    ExitModule("FormalParameter");
}

//-----------------------------------------------------------
void ParseMainProgram(TOKEN tokens[]) 
//-----------------------------------------------------------
{
    void ParseDataDefinitions(TOKEN tokens[], IDENTIFIERSCOPE identifierScope);
    void GetNextToken(TOKEN tokens[]);
    void ParseStatement(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    char label[SOURCELINELENGTH + 1];
    char reference[SOURCELINELENGTH + 1];

    EnterModule("MainProgram");

// CODEGENERATION
    code.EmitUnformattedLine("; **** =========");
    sprintf(line, "; **** 'main' program (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    code.EmitUnformattedLine("; **** =========");
    code.EmitFormattedLine("MAINPROGRAM", "EQU", "*");

    code.EmitFormattedLine("", "PUSH", "#RUNTIMESTACK", "set SP");
    code.EmitFormattedLine("", "POPSP");
    code.EmitFormattedLine("", "PUSHA", "STATICDATA", "set SB");
    code.EmitFormattedLine("", "POPSB");
    code.EmitFormattedLine("", "PUSH", "#HEAPBASE", "initialize heap");
    code.EmitFormattedLine("", "PUSH", "#HEAPSIZE");
    code.EmitFormattedLine("", "SVC", "#SVC_INITIALIZE_HEAP");
    sprintf(label, "MAINBODY%04d", code.LabelSuffix());
    code.EmitFormattedLine("", "CALL", label);
    code.AddDSToStaticData("Normal program termination", "", reference);
    code.EmitFormattedLine("", "PUSHA", reference);
    code.EmitFormattedLine("", "SVC", "#SVC_WRITE_STRING");
    code.EmitFormattedLine("", "SVC", "#SVC_WRITE_ENDL");
    code.EmitFormattedLine("", "PUSH", "#0D0", "terminate with status = 0");
    code.EmitFormattedLine("", "SVC", "#SVC_TERMINATE");
    code.EmitUnformattedLine("");
    code.EmitFormattedLine(label, "EQU", "*");
// ENDCODEGENERATION

    GetNextToken(tokens);

    identifierTable.EnterNestedStaticScope();

    while (tokens[0].type == ENUM)
        ParseEnumDeclaration(tokens, PROGRAMMODULESCOPE);

    ParseDataDefinitions(tokens, PROGRAMMODULESCOPE);

    while (tokens[0].type != ENDMAIN)
        ParseStatement(tokens);

// CODEGENERATION
    code.EmitFormattedLine("", "RETURN");
    code.EmitUnformattedLine("; **** =========");
    sprintf(line, "; **** 'end main' (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    code.EmitUnformattedLine("; **** =========");
// ENDCODEGENERATION

#ifdef TRACECOMPILER
    identifierTable.DisplayTableContents("Contents of identifier table at end of compilation of PROGRAM module definition");
#endif

    identifierTable.ExitNestedStaticScope();
    GetNextToken(tokens);

    ExitModule("MainProgram");
}

//-----------------------------------------------------------
void ParseStatement(TOKEN tokens[]) 
//-----------------------------------------------------------
{
   void ParseAssertion(TOKEN tokens[]);
   void ParsePrintStatement(TOKEN tokens[]);
   void ParseInputStatement(TOKEN tokens[]);
   void ParseAssignmentStatement(TOKEN tokens[]);
   void ParseIfStatement(TOKEN tokens[]);
   void ParseDoWhileStatement(TOKEN tokens[]);
   void ParseForStatement(TOKEN tokens[]);
   void ParseCallStatement(TOKEN tokens[]);
   void ParseReturnStatement(TOKEN tokens[]);
   void GetNextToken(TOKEN tokens[]);

/*** EDITING SPACE --- START ***/
   void ParseFilePrintStatement(TOKEN tokens[]);
   void ParseFileInputStatement(TOKEN tokens[]);

   EnterModule("Statement");

   while ( tokens[0].type == ASSERT )
       ParseAssertion(tokens);
   switch ( tokens[0].type )
   {
      case PRINT:
         ParsePrintStatement(tokens);
         break;
      case INPUT:
         ParseInputStatement(tokens);
         break;
      case IDENTIFIER:
         ParseAssignmentStatement(tokens);
         break;
      case IF:
         ParseIfStatement(tokens);
         break;
      case DO:
      case WHILE:
         ParseDoWhileStatement(tokens);
         break;
      case FOR:
         ParseForStatement(tokens);
         break;
      case CALL:
          ParseCallStatement(tokens);
          break;
      case RETURN:
          ParseReturnStatement(tokens);
          break;
      case F_PRINT:
          ParseFilePrintStatement(tokens);
          break;
      case F_INPUT:
          ParseFileInputStatement(tokens);
          break;
      default:
         ProcessCompilerError(tokens[0].sourceLineNumber,tokens[0].sourceLineIndex,
                              "Expecting beginning-of-statement");
         break;
   }
   while ( tokens[0].type == ASSERT )
       ParseAssertion(tokens);

 /*** EDITING SPACE --- END ***/

   ExitModule("Statement");
}

//-----------------------------------------------------------
void ParseAssertion(TOKEN tokens[]) 
//-----------------------------------------------------------
{
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    EnterModule("Assertion");

    sprintf(line, "; **** %4d: assertion", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);

    GetNextToken(tokens);

    if (tokens[0].type != L_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '(' ");
    GetNextToken(tokens);

    ParseExpression(tokens, datatype);

// STATICSEMANTICS
    if (datatype != BOOLEANTYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean expression");
// ENDSTATICSEMANTICS

// CODEGENERATION
/*
        SETT
        JMPT      E????
        PUSH      #0D(sourceLineNumber)
        PUSH      #0D1
        JMP       HANDLERUNTIMEERROR
E???? EQU       *
        DISCARD   #0D1
*/
    char Elabel[SOURCELINELENGTH + 1], operand[SOURCELINELENGTH + 1];

    code.EmitFormattedLine("", "SETT");
    sprintf(Elabel, "E%04d", code.LabelSuffix());
    code.EmitFormattedLine("", "JMPT", Elabel);
    sprintf(operand, "#0D%d", tokens[0].sourceLineNumber);
    code.EmitFormattedLine("", "PUSH", operand);
    code.EmitFormattedLine("", "PUSH", "#0D1");
    code.EmitFormattedLine("", "JMP", "HANDLERUNTIMEERROR");
    code.EmitFormattedLine(Elabel, "EQU", "*");
    code.EmitFormattedLine("", "DISCARD", "#0D1");
// ENDCODEGENERATION

    if (tokens[0].type != R_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
    GetNextToken(tokens);

    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");
    GetNextToken(tokens);
    
    ExitModule("Assertion");

}

//-----------------------------------------------------------
void ParsePrintStatement(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    EnterModule("PrintStatement");

    GetNextToken(tokens);

    // CODEGENERATION
    sprintf(line, "; **** 'print' statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    // ENDCODEGENERATION

       //Check if the next token is a left parentheses
    if (tokens[0].type != L_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting '('");

    do
    {
        GetNextToken(tokens);

        if (tokens[0].type == STRING) {
            // CODEGENERATION
            char reference[SOURCELINELENGTH + 1];

            code.AddDSToStaticData(tokens[0].lexeme, "", reference);
            code.EmitFormattedLine("", "PUSHA", reference);
            code.EmitFormattedLine("", "SVC", "#SVC_WRITE_STRING");
            // ENDCODEGENERATION

            GetNextToken(tokens);
        }
        else {
            ParseExpression(tokens, datatype);

            // CODEGENERATION
            switch (datatype)
            {
            case INTEGERTYPE:
            case ENUMTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_INTEGER");
                break;
            case BOOLEANTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_BOOLEAN");
                break;
            case CHARACTERTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_CHARACTER");
                break;
            case FLOATTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_FLOAT");
                break;
            case POINTERTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_HEXADECIMAL");
                break;
            }
            // ENDCODEGENERATION

        }
    } while (tokens[0].type == COMMA);

    //Check if the next token is a right parentheses
    if (tokens[0].type != R_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ')'");

    GetNextToken(tokens);

    //Check if the next token is a semicolon
    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ';'");

    GetNextToken(tokens);

    ExitModule("PrintStatement");
}

//-----------------------------------------------------------
void ParseInputStatement(TOKEN tokens[]) 
//-----------------------------------------------------------
{
   void ParseVariable(TOKEN tokens[],bool asLValue,DATATYPE &datatype);
   void GetNextToken(TOKEN tokens[]);

   char reference[SOURCELINELENGTH+1];
   char line[SOURCELINELENGTH+1];
   DATATYPE datatype;

   EnterModule("InputStatement");

   sprintf(line,"; **** 'input' statement (%4d)",tokens[0].sourceLineNumber);
   code.EmitUnformattedLine(line);

   GetNextToken(tokens);

   //Check if the next token is a left parentheses
   if (tokens[0].type != L_PAREN)
       ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
           "Expecting '('");
   GetNextToken(tokens);

   if ( tokens[0].type == STRING )
   {

// CODEGENERATION
      code.AddDSToStaticData(tokens[0].lexeme,"",reference);
      code.EmitFormattedLine("","PUSHA",reference);
      code.EmitFormattedLine("","SVC","#SVC_WRITE_STRING");
// ENDCODEGENERATION

      GetNextToken(tokens);

      //Check if the next token is a comma
      if (tokens[0].type != COMMA)
          ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
              "Expecting ','");
      GetNextToken(tokens);
   }

   ParseVariable(tokens,true,datatype);

// CODEGENERATION
   switch ( datatype )
   {
      case INTEGERTYPE:
         code.EmitFormattedLine("","SVC","#SVC_READ_INTEGER");
         break;
      case BOOLEANTYPE:
         code.EmitFormattedLine("","SVC","#SVC_READ_BOOLEAN");
         break;
      case CHARACTERTYPE:
          code.EmitFormattedLine("", "SVC", "#SVC_READ_CHARACTER");
          break;
      case FLOATTYPE:
          code.EmitFormattedLine("", "SVC", "#SVC_READ_FLOAT");
          break;
   }
   code.EmitFormattedLine("","POP","@SP:0D1");
   code.EmitFormattedLine("","DISCARD","#0D1");
// ENDCODEGENERATION

  //Check if the next token is a right parentheses
   if (tokens[0].type != R_PAREN)
       ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
           "Expecting ')'");
   GetNextToken(tokens);

   //Check if the next token is a semicolon
   if (tokens[0].type != SEMICOLON)
       ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
           "Expecting ';'");
   GetNextToken(tokens);

   ExitModule("InputStatement");
}

//-----------------------------------------------------------
void ParseAssignmentStatement(TOKEN tokens[]) /*** updated ***/
//-----------------------------------------------------------
{
   void ParseVariable(TOKEN tokens[],bool asLValue,DATATYPE &datatype);
   void ParseExpression(TOKEN tokens[],DATATYPE &datatype);
   void ParseAssociativeArrayReference(TOKEN tokens[], bool asLValue, DATATYPE & datatype);
   void GetNextToken(TOKEN tokens[]);

/*** EDITING SPACE -- START ***/
   void ParseFileOperationStatement(TOKEN tokens[]);
/*** EDITING SPACE -- END ***/


   char line[SOURCELINELENGTH+1];

   EnterModule("AssignmentStatement");

   sprintf(line,"; **** assignment statement (%4d)",tokens[0].sourceLineNumber);
   code.EmitUnformattedLine(line);

   int index;
   bool isInTable;
   DATATYPE datatype;

   // STATICSEMANTICS
   index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
   if (!isInTable)
       ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Undefined identifier");

   datatype = identifierTable.GetDatatype(index);

/*** EDITING SPACE --- START ***/
// <FileOperationStatement>
   if (datatype == FILETYPE)
   {
       ParseVariable(tokens, true, datatype);
       ParseFileOperationStatement(tokens);
   }
/*** EDITING SPACE --- END ***/

// <AssociativeArrayReference> = <expression>;
   else if (datatype == ASSOCTYPE)
   {
       DATATYPE valueDatatype;

       ParseAssociativeArrayReference(tokens, true, datatype);

       if (tokens[0].type != ASSIGN)
           ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '='");
       GetNextToken(tokens);

       ParseExpression(tokens, valueDatatype);

       // Check that the value is a scalar datatype
       if ((valueDatatype != INTEGERTYPE) && (valueDatatype != FLOATTYPE) &&
           (valueDatatype != CHARACTERTYPE) && (valueDatatype != BOOLEANTYPE))
       {
           ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
               "Value must be of a scalar datatype (int, bool, char, or float)");
       }

       //CODEGENERATION
       /*** Add the (key, value) pair to the associative array and increment the size ***/
       code.EmitFormattedLine("", "SWAP");
       code.EmitFormattedLine("", "SETAAE", identifierTable.GetReference(index));
       //ENDCODEGENERATION
   }

// <variableList> = <expression>;
   else
   {
       DATATYPE datatypeLHS,datatypeRHS; 
       int n;                            
       ParseVariable(tokens, true, datatypeLHS);
       n = 1;

       while (tokens[0].type == COMMA)
       {
           DATATYPE datatypeNEXT;
           int indexNEXT;

           GetNextToken(tokens);

           indexNEXT = identifierTable.GetIndex(tokens[0].lexeme, isInTable);

           ParseVariable(tokens, true, datatypeNEXT);
           n++;

           if (datatypeNEXT != datatypeLHS)
               ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Mixed-mode variables not allowed");

           if (datatypeNEXT == ENUMTYPE)
           {
               if (identifierTable.GetEnumType(indexNEXT) != identifierTable.GetEnumType(index))
                   ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Mixed-mode variables not allowed");       
           }
           
       }
       if (tokens[0].type != ASSIGN)
           ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '='");
       GetNextToken(tokens);
       
       // If the datatype of the LHS is an enumeration type
       if (datatype == ENUMTYPE)
       {
           // Save the scope and enum type of the l-value(s)
           switch (identifierTable.GetType(index))
           {
           case GLOBAL_VARIABLE:
               lValueEnumScope = GLOBALSCOPE;
               break;
           case PROGRAMMODULE_VARIABLE:
               lValueEnumScope = PROGRAMMODULESCOPE;
               break;
           case SUBPROGRAMMODULE_VARIABLE:
               lValueEnumScope = SUBPROGRAMMODULESCOPE;
               break;
           }

           lValueEnumType = identifierTable.GetEnumType(index);

           // and if the token about to be parsed is an identifier
           if (tokens[0].type == IDENTIFIER)
           {
               // Check if the identifier is an enum type
               int indexRHS = identifierTable.GetIndex(tokens[0].lexeme, isInTable);

               // If so, see if its enum type matches that of the LHS
               if (identifierTable.GetDatatype(indexRHS) == ENUMTYPE &&
                   identifierTable.GetEnumType(index) != identifierTable.GetEnumType(indexRHS))
               {
                   ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Data type mismatch");
               }
           }
       }

       ParseExpression(tokens, datatypeRHS);
       
       if (datatypeLHS != datatypeRHS)
           ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Data type mismatch");

       
       // CODEGENERATION
       for (int i = 1; i <= n; i++)
       {
           code.EmitFormattedLine("", "MAKEDUP");
           code.EmitFormattedLine("", "POP", "@SP:0D2");
           code.EmitFormattedLine("", "SWAP");
           code.EmitFormattedLine("", "DISCARD", "#0D1");
       }
       code.EmitFormattedLine("", "DISCARD", "#0D1");
       // ENDCODEGENERATION
   }

   if (tokens[0].type != SEMICOLON)
       ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");
   GetNextToken(tokens);

   // clear the vector of pointer indexes 
   ptr_indexes.clear(); 

   // reset the lValueEnumType to ""
   lValueEnumType = "";

   ExitModule("AssignmentStatement");

}

//-----------------------------------------------------------
void ParseIfStatement(TOKEN tokens[]) 
//-----------------------------------------------------------
{
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void ParseStatement(TOKEN tokens[]);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    char Ilabel[SOURCELINELENGTH + 1], Elabel[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    EnterModule("IfStatement");

    sprintf(line, "; **** 'if' statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);

    GetNextToken(tokens);

    if (tokens[0].type != L_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '('");
    GetNextToken(tokens);
    ParseExpression(tokens, datatype);
    if (tokens[0].type != R_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
    GetNextToken(tokens);

    if (datatype != BOOLEANTYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean expression");

// CODEGENERATION
/*
    Plan for the generalized IF statement with n ELIFs and 1 ELSE (*Note* n
        can be 0 and the ELSE may be missing and the plan still "works.")

    ...expression...           ; boolean expression on top-of-stack
        SETT
        DISCARD   #0D1
        JMPNT     I???1
    ...statements...
        JMP       E????
I???1 EQU       *             ; 1st ELIF clause
    ...expression...
        SETT
        DISCARD   #0D1
        JMPNT     I???2
    ...statements...
        JMP       E????
        .
        .
I???n EQU       *             ; nth ELIF clause
    ...expression...
        SETT
        DISCARD   #0D1
        JMPNT     I????
    ...statements...
        JMP       E????
I???? EQU       *             ; ELSE clause
    ...statements...
E???? EQU       *
*/
    sprintf(Elabel, "E%04d", code.LabelSuffix());
    code.EmitFormattedLine("", "SETT");
    code.EmitFormattedLine("", "DISCARD", "#0D1");
    sprintf(Ilabel, "I%04d", code.LabelSuffix());
    code.EmitFormattedLine("", "JMPNT", Ilabel);
// ENDCODEGENERATION

    while ( (tokens[0].type != ELIF) &&
            (tokens[0].type != ELSE) &&
            (tokens[0].type != ENDIF))
        ParseStatement(tokens);

// CODEGENERATION
    code.EmitFormattedLine("", "JMP", Elabel);
    code.EmitFormattedLine(Ilabel, "EQU", "*");
// ENDCODEGENERATION

    while (tokens[0].type == ELIF)
    {
        GetNextToken(tokens);
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '('");
        GetNextToken(tokens);
        ParseExpression(tokens, datatype);
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
        GetNextToken(tokens);

        if (datatype != BOOLEANTYPE)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean expression");

// CODEGENERATION
        code.EmitFormattedLine("", "SETT");
        code.EmitFormattedLine("", "DISCARD", "#0D1");
        sprintf(Ilabel, "I%04d", code.LabelSuffix());
        code.EmitFormattedLine("", "JMPNT", Ilabel);
// ENDCODEGENERATION

        while ((tokens[0].type != ELIF) &&
            (tokens[0].type != ELSE) &&
            (tokens[0].type != ENDIF))
            ParseStatement(tokens);

// CODEGENERATION
        code.EmitFormattedLine("", "JMP", Elabel);
        code.EmitFormattedLine(Ilabel, "EQU", "*");
// ENDCODEGENERATION
    }
    if (tokens[0].type == ELSE)
    {
        GetNextToken(tokens);
        while (tokens[0].type != ENDIF)
            ParseStatement(tokens);
    }

    GetNextToken(tokens);

// CODEGENERATION
    code.EmitFormattedLine(Elabel, "EQU", "*");
// ENDCODEGENERATION

    ExitModule("IfStatement");
}

//-----------------------------------------------------------
void ParseDoWhileStatement(TOKEN tokens[]) 
//-----------------------------------------------------------
{
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void ParseStatement(TOKEN tokens[]);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    char Dlabel[SOURCELINELENGTH + 1], Elabel[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    EnterModule("DoWhileStatement");

    sprintf(line, "; **** 'do/while' statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);

// CODEGENERATION
/*
D???? EQU       *
    ...statements...
    ...expression...
        SETT
        DISCARD   #0D1
        JMPNT     E????
    ...statements...
        JMP       D????
E???? EQU       *
*/

    sprintf(Dlabel, "D%04d", code.LabelSuffix());
    sprintf(Elabel, "E%04d", code.LabelSuffix());
    code.EmitFormattedLine(Dlabel, "EQU", "*");
// ENDCODEGENERATION

    if (tokens[0].type == DO) {
        GetNextToken(tokens);

        while (tokens[0].type != WHILE)
            ParseStatement(tokens);
    }

    GetNextToken(tokens);

    if (tokens[0].type != L_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '('");
    GetNextToken(tokens);
    ParseExpression(tokens, datatype);
    if (tokens[0].type != R_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
    GetNextToken(tokens);

    if (datatype != BOOLEANTYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean expression");

// CODEGENERATION
    code.EmitFormattedLine("", "SETT");
    code.EmitFormattedLine("", "DISCARD", "#0D1");
    code.EmitFormattedLine("", "JMPNT", Elabel);
// ENDCODEGENERATION

    while (tokens[0].type != ENDWHILE)
        ParseStatement(tokens);

    GetNextToken(tokens);

// CODEGENERATION
    code.EmitFormattedLine("", "JMP", Dlabel);
    code.EmitFormattedLine(Elabel, "EQU", "*");
// ENDCODEGENERATION

    ExitModule("DoWhileStatement");
}

//-----------------------------------------------------------
void ParseForStatement(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseVariable(TOKEN tokens[], bool asLValue, DATATYPE & datatype);
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void ParseStatement(TOKEN tokens[]);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    char Dlabel[SOURCELINELENGTH + 1], Llabel[SOURCELINELENGTH + 1],
        Clabel[SOURCELINELENGTH + 1], Elabel[SOURCELINELENGTH + 1];
    char operand[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    EnterModule("ForStatement");

    sprintf(line, "; **** 'for' statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);

    GetNextToken(tokens);

    ParseVariable(tokens, true, datatype);

    if (datatype != INTEGERTYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer variable");

/*
; v := e1
     ...v...                    ; &v = run-time stack (bottom to top)
     ...e1...                   ; &v,e1
        POP       @SP:0D1       ; &v
     ...e2...                   ; &v,e2
     ...e3...                   ; &v,e2,e3
        SETNZPI
; if ( e3 = 0 ) then
        JMPNZ     D????
        PUSH      #0D(current line number)
        PUSH      #0D2
        JMP       HANDLERUNTIMEERROR
D????   SETNZPI
; else if ( e3 > 0 ) then
        JMPN      L????
        SWAP                    ; &v,e3,e2
        MAKEDUP                 ; &v,e3,e2,e2
        PUSH      @SP:0D3       ; &v,e3,e2,e2,v
        SWAP                    ; &v,e3,e2,v,e2
;    if ( v <= e2 ) continue else end
        CMPI                    ; &v,e3,e2 (set LEG)
        JMPLE     C????
        JMP       E????
; else ( e3 < 0 )
L????   SWAP                    ; &v,e3,e2
        MAKEDUP                 ; &v,e3,e2,e2
        PUSH      @SP:0D3       ; &v,e3,e2,e2,v
        SWAP                    ; &v,e3,e2,v,e2
;    if ( v >= e2 ) continue else end
        CMPI                    ; &v,e3,e2 (set LEG)
        JMPGE     C????
        JMP       E????
; endif
C????   EQU       *
    ...statements...
        SWAP                    ; &v,e2,e3
        MAKEDUP                 ; &v,e2,e3,e3
; v := e3+v
        PUSH      @SP:0D3       ; &v,e2,e3,e3,v
        ADDI                    ; &v,e2,e3,(e3+v)
        POP       @SP:0D3       ; &v,e2,e3
        JMP       D????
E????   DISCARD   #0D3          ; now run-time stack is empty
*/

    if (tokens[0].type != ASSIGN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '='");
    GetNextToken(tokens);

    ParseExpression(tokens, datatype);
    if (datatype != INTEGERTYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer data type");

// CODEGENERATION
    code.EmitFormattedLine("", "POP", "@SP:0D1");
// ENDCODEGENERATION

    if (tokens[0].type != TO)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting TO");
    GetNextToken(tokens);

    ParseExpression(tokens, datatype);
    if (datatype != INTEGERTYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer data type");

    if (tokens[0].type == BY)
    {
        GetNextToken(tokens);

        ParseExpression(tokens, datatype);
        if (datatype != INTEGERTYPE)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer data type");
    }
    else
    {

// CODEGENERATION
        code.EmitFormattedLine("", "PUSH", "#0D1");
// ENDCODEGENERATION

    }

// CODEGENERATION
    sprintf(Dlabel, "D%04d", code.LabelSuffix());
    sprintf(Llabel, "L%04d", code.LabelSuffix());
    sprintf(Clabel, "C%04d", code.LabelSuffix());
    sprintf(Elabel, "E%04d", code.LabelSuffix());

    code.EmitFormattedLine("", "SETNZPI");
    code.EmitFormattedLine("", "JMPNZ", Dlabel);
    sprintf(operand, "#0D%d", tokens[0].sourceLineNumber);
    code.EmitFormattedLine("", "PUSH", operand);
    code.EmitFormattedLine("", "PUSH", "#0D2");
    code.EmitFormattedLine("", "JMP", "HANDLERUNTIMEERROR");

    code.EmitFormattedLine(Dlabel, "SETNZPI");
    code.EmitFormattedLine("", "JMPN", Llabel);
    code.EmitFormattedLine("", "SWAP");
    code.EmitFormattedLine("", "MAKEDUP");
    code.EmitFormattedLine("", "PUSH", "@SP:0D3");
    code.EmitFormattedLine("", "SWAP");
    code.EmitFormattedLine("", "CMPI");
    code.EmitFormattedLine("", "JMPLE", Clabel);
    code.EmitFormattedLine("", "JMP", Elabel);
    code.EmitFormattedLine(Llabel, "SWAP");
    code.EmitFormattedLine("", "MAKEDUP");
    code.EmitFormattedLine("", "PUSH", "@SP:0D3");
    code.EmitFormattedLine("", "SWAP");
    code.EmitFormattedLine("", "CMPI");
    code.EmitFormattedLine("", "JMPGE", Clabel);
    code.EmitFormattedLine("", "JMP", Elabel);
    code.EmitFormattedLine(Clabel, "EQU", "*");
// ENDCODEGENERATION

    while (tokens[0].type != ENDFOR)
        ParseStatement(tokens);

    GetNextToken(tokens);

// CODEGENERATION
    code.EmitFormattedLine("", "SWAP");
    code.EmitFormattedLine("", "MAKEDUP");
    code.EmitFormattedLine("", "PUSH", "@SP:0D3");
    code.EmitFormattedLine("", "ADDI");
    code.EmitFormattedLine("", "POP", "@SP:0D3");
    code.EmitFormattedLine("", "JMP", Dlabel);
    code.EmitFormattedLine(Elabel, "DISCARD", "#0D3");
// ENDCODEGENERATION

    ExitModule("ForStatement");
}

//-----------------------------------------------------------
void ParseCallStatement(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseVariable(TOKEN tokens[], bool asLValue, DATATYPE & datatype);
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    bool isInTable;
    int index, parameters;

    EnterModule("CallStatement");

    sprintf(line, "; **** Call statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);

    GetNextToken(tokens);

    if (tokens[0].type != IDENTIFIER)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");

// STATICSEMANTICS
    index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
    if (!isInTable)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Undefined identifier");
    if (identifierTable.GetType(index) != PROCEDURE_SUBPROGRAMMODULE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting PROCEDURE identifier");
// ENDSTATICSEMANTICS

    GetNextToken(tokens);
    parameters = 0;
    if (tokens[0].type == L_PAREN)
    {
        DATATYPE expressionDatatype, variableDatatype;

        do
        {
            GetNextToken(tokens);
            parameters++;

// CODEGENERATION   
// STATICSEMANTICS
            switch (identifierTable.GetType(index + parameters))
            {
            case IN_PARAMETER:
                ParseExpression(tokens, expressionDatatype);
                if (expressionDatatype != identifierTable.GetDatatype(index + parameters))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                        "Actual parameter data type does not match formal parameter data type");
                break;
            case OUT_PARAMETER:
                ParseVariable(tokens, true, variableDatatype);
                if (variableDatatype != identifierTable.GetDatatype(index + parameters))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                        "Actual parameter data type does not match formal parameter data type");
                code.EmitFormattedLine("", "PUSH", "#0X0000");
                break;
            case IO_PARAMETER:
                ParseVariable(tokens, true, variableDatatype);
                if (variableDatatype != identifierTable.GetDatatype(index + parameters))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                        "Actual parameter data type does not match formal parameter data type");
                code.EmitFormattedLine("", "PUSH", "@SP:0D0");
                break;
            case REF_PARAMETER:
                ParseVariable(tokens, true, variableDatatype);
                if (variableDatatype != identifierTable.GetDatatype(index + parameters))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                        "Actual parameter data type does not match formal parameter data type");
                break;
            }
// ENDSTATICSEMANTICS
// ENDCODEGENERATION
        } while (tokens[0].type == COMMA);

        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting )");

        GetNextToken(tokens);
    }

// STATICSEMANTICS
    if (identifierTable.GetCountOfFormalParameters(index) != parameters)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Number of actual parameters does not match number of formal parameters");
// ENDSTATICSEMANTICS

// CODEGENERATION
    code.EmitFormattedLine("", "PUSHFB");
    code.EmitFormattedLine("", "CALL", identifierTable.GetReference(index));
    code.EmitFormattedLine("", "POPFB");
    for (parameters = identifierTable.GetCountOfFormalParameters(index); parameters >= 1; parameters--)
    {
        switch (identifierTable.GetType(index + parameters))
        {
        case IN_PARAMETER:
            code.EmitFormattedLine("", "DISCARD", "#0D1");
            break;
        case OUT_PARAMETER:
            code.EmitFormattedLine("", "POP", "@SP:0D1");
            code.EmitFormattedLine("", "DISCARD", "#0D1");
            break;
        case IO_PARAMETER:
            code.EmitFormattedLine("", "POP", "@SP:0D1");
            code.EmitFormattedLine("", "DISCARD", "#0D1");
            break;
        case REF_PARAMETER:
            code.EmitFormattedLine("", "DISCARD", "#0D1");
            break;
        }
    }
// ENDCODEGENERATION

    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");

    GetNextToken(tokens);

    ExitModule("CallStatement");
}

//-----------------------------------------------------------
void ParseReturnStatement(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];

    EnterModule("ReturnStatement");

    sprintf(line, "; **** Return statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);

    GetNextToken(tokens);

// STATICSEMANTICS
    if (code.IsInModuleBody(PROCEDURE_SUBPROGRAMMODULE))
// CODEGENERATION
        code.EmitFormattedLine("", "RETURN");
// ENDCODEGENERATION
    else if (code.IsInModuleBody(FUNCTION_SUBPROGRAMMODULE))
    {
        DATATYPE datatype;

        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '('");
        GetNextToken(tokens);

        ParseExpression(tokens, datatype);

        if (datatype != identifierTable.GetDatatype(code.GetModuleIdentifierIndex()))
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Return expression data type must match FUNCTION ('func') data type");

// CODEGENERATION
        code.EmitFormattedLine("", "POP", "FB:0D0", "pop RETURN expression into function return value");
        code.EmitFormattedLine("", "RETURN");
// ENDCODEGENERATION

        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
        GetNextToken(tokens);
    }
    else
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Return statement only allowed in PROCEDURE ('proc') or FUNCTION ('func') module body");
    // ENDSTATICSEMANTICS

    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ';'");

    GetNextToken(tokens);

    ExitModule("ReturnStatement");
}

/*** EDITING SPACE --- START ***/
// TODO: Replace the CODEGENERATION segments with 
//  service requests for file I/O
//-----------------------------------------------------------
void ParseFilePrintStatement(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    bool isInTable;
    int index;

    EnterModule("FilePrintStatement");

    // Get the next token
    GetNextToken(tokens);

    // CODEGENERATION
    sprintf(line, "; **** 'f_print' statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    // ENDCODEGENERATION

    // Check for a left parenthesis: '('
    if (tokens[0].type != L_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting '('");

    // Get the next token
    GetNextToken(tokens);

    // Check if the next token is an identifier
    if ((tokens[0].type != IDENTIFIER))
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting identifier");
    
    // Check if the identifier is in the identifier table
    index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
    if (!isInTable)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Undefined identifier");

    // Check if the identifier is an file variable
    if (identifierTable.GetDatatype(index) != FILETYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting file variable");

    // Get the next token
    GetNextToken(tokens);

    // Check for a comma
    if (tokens[0].type != COMMA)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ','");

    do
    {
        GetNextToken(tokens);

        // <string>
        if (tokens[0].type == STRING) {
            // CODEGENERATION
            /*
            char reference[SOURCELINELENGTH + 1];

            code.AddDSToStaticData(tokens[0].lexeme, "", reference);
            code.EmitFormattedLine("", "PUSHA", reference);
            code.EmitFormattedLine("", "SVC", "#SVC_WRITE_STRING");
            */
            // ENDCODEGENERATION

            GetNextToken(tokens);
        }
        // <expression>
        else {
            ParseExpression(tokens, datatype);

            // CODEGENERATION
            /*
            switch (datatype)
            {
            case INTEGERTYPE:
            case ENUMTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_INTEGER");
                break;
            case BOOLEANTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_BOOLEAN");
                break;
            case CHARACTERTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_CHARACTER");
                break;
            case FLOATTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_FLOAT");
                break;
            case POINTERTYPE:
                code.EmitFormattedLine("", "SVC", "#SVC_WRITE_HEXADECIMAL");
                break;
            }
            */
            // ENDCODEGENERATION

        }
    } while (tokens[0].type == COMMA);

    //Check for a right parenthesis: ')'
    if (tokens[0].type != R_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ')'");

    // Get the next token
    GetNextToken(tokens);

    // Check for a semicolon
    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ';'");

    // Get the next token
    GetNextToken(tokens);

    ExitModule("FilePrintStatement");
}

//-----------------------------------------------------------
void ParseFileInputStatement(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseVariable(TOKEN tokens[], bool asLValue, DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    bool isInTable;
    int index;

    EnterModule("FileInputStatement");

    // Get the next token
    GetNextToken(tokens);

    // CODEGENERATION
    sprintf(line, "; **** 'f_input' statement (%4d)", tokens[0].sourceLineNumber);
    code.EmitUnformattedLine(line);
    // ENDCODEGENERATION

    // Check for a left parenthesis
    if (tokens[0].type != L_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting '('");

    // Get the next token
    GetNextToken(tokens);

    // Check if the next token is an identifier
    if ((tokens[0].type != IDENTIFIER))
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting identifier");

    // Check if the identifier is in the identifier table
    index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
    if (!isInTable)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Undefined identifier");

    // Check if the identifier is an file variable
    if (identifierTable.GetDatatype(index) != FILETYPE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting file variable");

    // Get the next token
    GetNextToken(tokens);

    // Check for a comma
    if (tokens[0].type != COMMA)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ','");

    // Get the next token
    GetNextToken(tokens);

    // Parse the variable
    ParseVariable(tokens, true, datatype);

    // CODEGENERATION
    /*
    switch (datatype)
    {
    case INTEGERTYPE:
        code.EmitFormattedLine("", "SVC", "#SVC_READ_INTEGER");
        break;
    case BOOLEANTYPE:
        code.EmitFormattedLine("", "SVC", "#SVC_READ_BOOLEAN");
        break;
    case CHARACTERTYPE:
        code.EmitFormattedLine("", "SVC", "#SVC_READ_CHARACTER");
        break;
    case FLOATTYPE:
        code.EmitFormattedLine("", "SVC", "#SVC_READ_FLOAT");
        break;
    }
    code.EmitFormattedLine("", "POP", "@SP:0D1");
    code.EmitFormattedLine("", "DISCARD", "#0D1");
    */
    // ENDCODEGENERATION

    //Check for a right parenthesis
    if (tokens[0].type != R_PAREN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ')'");

    // Get the next token
    GetNextToken(tokens);

    //Check for a semicolon
    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ';'");

    // Get the next token
    GetNextToken(tokens);

    ExitModule("FileInputStatement");
}

//-----------------------------------------------------------
void ParseFileOperationStatement(TOKEN tokens[])
//-----------------------------------------------------------
{
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    char line[SOURCELINELENGTH + 1];
    DATATYPE datatype;

    bool isInTable;
    int index;

    EnterModule("FileOperationStatement");

    // Get the next token
    //GetNextToken(tokens);

    // Check for assigment operator: '='
    if (tokens[0].type != ASSIGN)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting '='");

    // Get the next token
    GetNextToken(tokens);

    // Check the file operation 
    // TODO: Add code generation
    switch (tokens[0].type)
    {
    case F_CREATE:
        sprintf(line, "; **** f_create statement (%4d)", tokens[0].sourceLineNumber);
        code.EmitUnformattedLine(line);

        // Get the next token
        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting '('");

        // Get the next token
        GetNextToken(tokens);

        // Check for <string>
        if (tokens[0].type != STRING)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting a string argument");

        // CODEGENERATION
        char reference[SOURCELINELENGTH + 1];
        
        code.AddDSToStaticData(tokens[0].lexeme, "", reference);

        // Point the file variable to the new file
        code.EmitFormattedLine("", "PUSHA", reference);
        code.EmitFormattedLine("", "POP", "@SP:0D1");
        code.EmitFormattedLine("", "DISCARD", "#0D1");

        // Create the new file  
        code.EmitFormattedLine("", "PUSHA", reference);
        code.EmitFormattedLine("", "SVC", "#SVC_CREATE_FILE");
        
        // ENDCODEGENERATION

        // Get the next token
        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting ')'");

        break;
    case F_READ:
        sprintf(line, "; **** f_read statement (%4d)", tokens[0].sourceLineNumber);
        code.EmitUnformattedLine(line);

        // Get the next token
        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting '('");

        // Get the next token
        GetNextToken(tokens);

        switch (tokens[0].type)
        {
        case TRUE:
            // CODEGENERATION
            // ...
            // ENDCODEGENERATION
            break;
        case FALSE:
            // CODEGENERATION
            // ...
            // ENDCODEGENERATION
            break;
        default:
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting 'true' or 'false");
        }

        // Get the next token
        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting ')'");

        break;
    case F_WRITE:
        sprintf(line, "; **** f_write statement (%4d)", tokens[0].sourceLineNumber);
        code.EmitUnformattedLine(line);
        // Get the next token
        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting '('");

        // Get the next token
        GetNextToken(tokens);

        switch (tokens[0].type)
        {
        case TRUE:
            // CODEGENERATION
            // ...
            // ENDCODEGENERATION
            break;
        case FALSE:
            // CODEGENERATION
            // ...
            // ENDCODEGENERATION
            break;
        default:
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting 'true' or 'false");
        }

        // Get the next token
        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting ')'");

        break;
    case F_CLEAR:
        sprintf(line, "; **** f_clear statement (%4d)", tokens[0].sourceLineNumber);
        code.EmitUnformattedLine(line);
        // Get the next token
        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting '('");

        // Get the next token
        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting ')'");

        break;
    case F_OPEN:
        sprintf(line, "; **** f_open statement (%4d)", tokens[0].sourceLineNumber);
        code.EmitUnformattedLine(line);

        // Get the next token
        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting '('");

        // Get the next token
        GetNextToken(tokens);

        // Check for <string>
        if (tokens[0].type != STRING)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting a string argument");

        // CODEGENERATION
        // char reference[SOURCELINELENGTH + 1];
        // ...
        // ENDCODEGENERATION

        // Get the next token
        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting ')'");

        break;
    case F_CLOSE:
        sprintf(line, "; **** f_close statement (%4d)", tokens[0].sourceLineNumber);
        code.EmitUnformattedLine(line);
        // Get the next token
        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting '('");

        // Get the next token
        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting ')'");

        break;
    case F_DELETE:
        sprintf(line, "; **** f_delete statement (%4d)", tokens[0].sourceLineNumber);
        code.EmitUnformattedLine(line);
        // Get the next token
        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting '('");

        // Get the next token
        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                "Expecting ')'");

        break;
    default:
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting a file operation");
    }

    // Get the next token
    GetNextToken(tokens);

    // Check for semicolon
    if (tokens[0].type != SEMICOLON)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Expecting ';'");

    ExitModule("FileOperationStatement");
}

//-----------------------------------------------------------
void ParseFCreate(TOKEN tokens[])
//-----------------------------------------------------------
{
}



/*** EDITING SPACE --- END ***/



//-----------------------------------------------------------
void ParseExpression(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    // CODEGENERATION
    /*
        An expression is composed of a collection of one or more operands (Rogue calls them
            primaries) and operators (and perhaps sets of parentheses to modify the default
            order-of-evaluation established by precedence and associativity rules).
            Expression evaluation computes a single value as the expression's result.
            The result has a specific data type. By design, the expression result is
            "left" at the top of the run-time stack for subsequent use.

        Rogue expressions must be single-mode with operators working on operands of
            the appropriate type (for example, boolean AND boolean) and not mixing
            modes. Static semantic analysis guarantees that operators are
            operating on operands of appropriate data type.
    */
    // ENDCODEGENERATION

    void ParseConjunction(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    DATATYPE datatypeLHS, datatypeRHS;

    EnterModule("Expression");

    ParseConjunction(tokens, datatypeLHS);

    if ((tokens[0].type == OR) ||
        (tokens[0].type == NOR) ||
        (tokens[0].type == XOR))
    {
        while ((tokens[0].type == OR) ||
            (tokens[0].type == NOR) ||
            (tokens[0].type == XOR))
        {
            TOKENTYPE operation = tokens[0].type;

            GetNextToken(tokens);
            ParseConjunction(tokens, datatypeRHS);

            // CODEGENERATION
            switch (operation)
            {
            case OR:

                // STATICSEMANTICS
                if (!((datatypeLHS == BOOLEANTYPE) && (datatypeRHS == BOOLEANTYPE)))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean operands");
                // ENDSTATICSEMANTICS

                code.EmitFormattedLine("", "OR");
                datatype = BOOLEANTYPE;
                break;
            case NOR:

                // STATICSEMANTICS
                if (!((datatypeLHS == BOOLEANTYPE) && (datatypeRHS == BOOLEANTYPE)))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean operands");
                // ENDSTATICSEMANTICS

                code.EmitFormattedLine("", "NOR");
                datatype = BOOLEANTYPE;
                break;
            case XOR:

                // STATICSEMANTICS
                if (!((datatypeLHS == BOOLEANTYPE) && (datatypeRHS == BOOLEANTYPE)))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean operands");
                // ENDSTATICSEMANTICS

                code.EmitFormattedLine("", "XOR");
                datatype = BOOLEANTYPE;
                break;
            }
        }
        // CODEGENERATION

    }
    else
        datatype = datatypeLHS;

    ExitModule("Expression");
}

//-----------------------------------------------------------
void ParseConjunction(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    void ParseNegation(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    DATATYPE datatypeLHS, datatypeRHS;

    EnterModule("Conjunction");

    ParseNegation(tokens, datatypeLHS);

    if ((tokens[0].type == AND) ||
        (tokens[0].type == NAND))
    {
        while ((tokens[0].type == AND) ||
            (tokens[0].type == NAND))
        {
            TOKENTYPE operation = tokens[0].type;

            GetNextToken(tokens);
            ParseNegation(tokens, datatypeRHS);

            switch (operation)
            {
            case AND:
                if (!((datatypeLHS == BOOLEANTYPE) && (datatypeRHS == BOOLEANTYPE)))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean operands");
                code.EmitFormattedLine("", "AND");
                datatype = BOOLEANTYPE;
                break;
            case NAND:
                if (!((datatypeLHS == BOOLEANTYPE) && (datatypeRHS == BOOLEANTYPE)))
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean operands");
                code.EmitFormattedLine("", "NAND");
                datatype = BOOLEANTYPE;
                break;
            }
        }
    }
    else
        datatype = datatypeLHS;

    ExitModule("Conjunction");
}

//-----------------------------------------------------------
void ParseNegation(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    void ParseComparison(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    DATATYPE datatypeRHS;

    EnterModule("Negation");

    if (tokens[0].type == NOT)
    {
        GetNextToken(tokens);
        ParseComparison(tokens, datatypeRHS);

        if (!(datatypeRHS == BOOLEANTYPE))
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting boolean operand");
        code.EmitFormattedLine("", "NOT");
        datatype = BOOLEANTYPE;
    }
    else
        ParseComparison(tokens, datatype);

    ExitModule("Negation");
}

//-----------------------------------------------------------
void ParseComparison(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    void ParseComparator(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    DATATYPE datatypeLHS, datatypeRHS;

    EnterModule("Comparison");

    ParseComparator(tokens, datatypeLHS);
    if ((tokens[0].type == LT) ||
        (tokens[0].type == LTEQ) ||
        (tokens[0].type == EQ) ||
        (tokens[0].type == GT) ||
        (tokens[0].type == GTEQ) ||
        (tokens[0].type == NOTEQ)
        )
    {
        TOKENTYPE operation = tokens[0].type;

        GetNextToken(tokens);
        ParseComparator(tokens, datatypeRHS);

        /*
              CMPI                    ; or CMPF (as required)
              JMPXX     T????         ; XX = L,E,G,LE,NE,GE (as required)
              PUSH      #0X0000       ; push FALSE
              JMP       E????         ;    or
        T???? PUSH      #0XFFFF       ; push TRUE (as required)
        E???? EQU       *
        */
              
        if ( ((datatypeLHS == INTEGERTYPE) && (datatypeRHS == INTEGERTYPE)) ||
             ((datatypeLHS == ENUMTYPE   ) && (datatypeRHS == INTEGERTYPE)) || 
             ((datatypeLHS == INTEGERTYPE) && (datatypeRHS == ENUMTYPE   )) || 
             ((datatypeLHS == ENUMTYPE   ) && (datatypeRHS == ENUMTYPE   )) )
        {
            code.EmitFormattedLine("", "CMPI");
        }
        else if ((datatypeLHS == FLOATTYPE) && (datatypeRHS == FLOATTYPE))
        {
            code.EmitFormattedLine("", "CMPF");
        }
        else if ( ((datatypeLHS == INTEGERTYPE) && (datatypeRHS == FLOATTYPE  )) ||
                  ((datatypeLHS == FLOATTYPE  ) && (datatypeRHS == INTEGERTYPE)) ||
                  ((datatypeLHS == ENUMTYPE   ) && (datatypeRHS == FLOATTYPE  )) ||
                  ((datatypeLHS == FLOATTYPE  ) && (datatypeRHS == ENUMTYPE   ))  )
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, 
                "Operands must be both integers, both enum types, one integer and one enum type, or both float");
        }
        else
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer, enum type, or float operands");
        }
        
        char Tlabel[SOURCELINELENGTH + 1], Elabel[SOURCELINELENGTH + 1];

        
        sprintf(Tlabel, "T%04d", code.LabelSuffix());
        sprintf(Elabel, "E%04d", code.LabelSuffix());
        switch (operation)
        {
        case LT:
            code.EmitFormattedLine("", "JMPL", Tlabel);
            break;
        case LTEQ:
            code.EmitFormattedLine("", "JMPLE", Tlabel);
            break;
        case EQ:
            code.EmitFormattedLine("", "JMPE", Tlabel);
            break;
        case GT:
            code.EmitFormattedLine("", "JMPG", Tlabel);
            break;
        case GTEQ:
            code.EmitFormattedLine("", "JMPGE", Tlabel);
            break;
        case NOTEQ:
            code.EmitFormattedLine("", "JMPNE", Tlabel);
            break;
        }
        datatype = BOOLEANTYPE;
        code.EmitFormattedLine("", "PUSH", "#0X0000");
        code.EmitFormattedLine("", "JMP", Elabel);
        code.EmitFormattedLine(Tlabel, "PUSH", "#0XFFFF");
        code.EmitFormattedLine(Elabel, "EQU", "*");
    }
    else
        datatype = datatypeLHS;

    ExitModule("Comparison");
}

//-----------------------------------------------------------
void ParseComparator(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    void ParseTerm(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    DATATYPE datatypeLHS, datatypeRHS;

    EnterModule("Comparator");

    ParseTerm(tokens, datatypeLHS);

    if ((tokens[0].type == PLUS) ||
        (tokens[0].type == MINUS))
    {
        while ((tokens[0].type == PLUS) ||
            (tokens[0].type == MINUS))
        {
            TOKENTYPE operation = tokens[0].type;

            GetNextToken(tokens);
            ParseTerm(tokens, datatypeRHS);

            if ((datatypeLHS == INTEGERTYPE) && (datatypeRHS == INTEGERTYPE))
            {
                switch (operation)
                {
                case PLUS:
                    code.EmitFormattedLine("", "ADDI");
                    break;
                case MINUS:
                    code.EmitFormattedLine("", "SUBI");
                    break;
                }
                datatype = INTEGERTYPE;
            }
            else if ((datatypeLHS == FLOATTYPE) && (datatypeRHS == FLOATTYPE))
            {
                switch (operation)
                {
                case PLUS:
                    code.EmitFormattedLine("", "ADDF");
                    break;
                case MINUS:
                    code.EmitFormattedLine("", "SUBF");
                    break;
                }
                datatype = FLOATTYPE;
            }
            else if ( (datatypeLHS == INTEGERTYPE) && (datatypeRHS == FLOATTYPE)
                       || ((datatypeLHS == FLOATTYPE) && (datatypeRHS == INTEGERTYPE)) )
            {
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Operands must be of the same datatype");
            }
            else
            {
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer or float operands");
            }
        }
    }
    else
        datatype = datatypeLHS;

    ExitModule("Comparator");
}

//-----------------------------------------------------------
void ParseTerm(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    void ParseFactor(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    DATATYPE datatypeLHS, datatypeRHS;

    EnterModule("Term");

    ParseFactor(tokens, datatypeLHS);
    if ((tokens[0].type == MULTIPLY) ||
        (tokens[0].type == DIVIDE) ||
        (tokens[0].type == MODULUS))
    {
        while ((tokens[0].type == MULTIPLY) ||
            (tokens[0].type == DIVIDE) ||
            (tokens[0].type == MODULUS))
        {
            TOKENTYPE operation = tokens[0].type;

            GetNextToken(tokens);
            ParseFactor(tokens, datatypeRHS);

            if ((datatypeLHS == INTEGERTYPE) && (datatypeRHS == INTEGERTYPE))
            {
                switch (operation)
                {
                case MULTIPLY:
                    code.EmitFormattedLine("", "MULI");
                    break;
                case DIVIDE:
                    code.EmitFormattedLine("", "DIVI");
                    break;
                case MODULUS:
                    code.EmitFormattedLine("", "REMI");
                    break;
                }
                datatype = INTEGERTYPE;
            }
            else if ((datatypeLHS == FLOATTYPE) && (datatypeRHS == FLOATTYPE))
            {
                switch (operation)
                {
                case MULTIPLY:
                    code.EmitFormattedLine("", "MULF");
                    break;
                case DIVIDE:
                    code.EmitFormattedLine("", "DIVF");
                    break;
                case MODULUS: /* the % operator only accepts integer operands */
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer operands");
                    break;
                }
                datatype = FLOATTYPE;
            }
            else if ((datatypeLHS == INTEGERTYPE) && (datatypeRHS == FLOATTYPE) ||
                (datatypeLHS == FLOATTYPE) && (datatypeRHS == INTEGERTYPE))
            {

                switch (operation)
                {
                case MODULUS:
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer operands");
                    break;
                default:
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Operands must be of the same datatype");
                    break;
                }
            }
            else
            {
                switch (operation)
                {
                case MODULUS:
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer operands");
                    break;
                default:
                    ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer or float operands");
                    break;
                }

            }
        }
    }
    else
        datatype = datatypeLHS;

    ExitModule("Term");
}

//-----------------------------------------------------------
void ParseFactor(TOKEN tokens[], DATATYPE& datatype)
//-----------------------------------------------------------
{
    void ParseSecondary(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    EnterModule("Factor");

    if ((tokens[0].type == ABS) ||
        (tokens[0].type == PLUS) ||
        (tokens[0].type == MINUS)
        )
    {
        DATATYPE datatypeRHS;
        TOKENTYPE operation = tokens[0].type;

        GetNextToken(tokens);
        ParseSecondary(tokens, datatypeRHS);
      
        if (datatypeRHS != INTEGERTYPE && datatypeRHS != FLOATTYPE)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer or float operand");

        switch (operation)
        {
        case ABS:
            /*
                    SETNZPI                 ; or SETNZPF (as required)
                    JMPNN     E????
                    NEGI                    ; NEGI or NEGF (as required)
            E???? EQU       *
            */
        {
            char Elabel[SOURCELINELENGTH + 1];

            sprintf(Elabel, "E%04d", code.LabelSuffix());

            switch (datatypeRHS)
            {
            case INTEGERTYPE:
                code.EmitFormattedLine("", "SETNZPI");
                code.EmitFormattedLine("", "JMPNN", Elabel);
                code.EmitFormattedLine("", "NEGI");
                break;
            case FLOATTYPE:
                code.EmitFormattedLine("", "SETNZPF");
                code.EmitFormattedLine("", "JMPNN", Elabel);
                code.EmitFormattedLine("", "NEGF");
                break;
            }
            
            code.EmitFormattedLine(Elabel, "EQU", "*");
        }
        break;
        case PLUS:
            // Do nothing (identity operator)
            break;
        case MINUS:
    
            switch (datatypeRHS)
            {
            case INTEGERTYPE:
                code.EmitFormattedLine("", "NEGI");
                break;
            case FLOATTYPE:
                code.EmitFormattedLine("", "NEGF");
                break;
            }
            break;
        }

        datatype = datatypeRHS;
    }
    else
        ParseSecondary(tokens, datatype);

    ExitModule("Factor");
}

//-----------------------------------------------------------
void ParseSecondary(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    void ParsePrefix(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    DATATYPE datatypeLHS, datatypeRHS;

    EnterModule("Secondary");

    ParsePrefix(tokens, datatypeLHS);

    if (tokens[0].type == POWER)
    {
        GetNextToken(tokens);

        ParsePrefix(tokens, datatypeRHS);

        if ((datatypeLHS == INTEGERTYPE) && (datatypeRHS == INTEGERTYPE))
        {
            code.EmitFormattedLine("", "POWI");
            datatype = INTEGERTYPE;
        }
        else if ((datatypeLHS == FLOATTYPE) && (datatypeRHS == FLOATTYPE))
        {
            code.EmitFormattedLine("", "POWF");
            datatype = FLOATTYPE;
        }
        else
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting 2 integer operands or 2 float operands");
        }
       
    }
    else
        datatype = datatypeLHS;

    ExitModule("Secondary");
}

//-----------------------------------------------------------
void ParsePrefix(TOKEN tokens[], DATATYPE& datatype) 
//-----------------------------------------------------------
{
    void ParseVariable(TOKEN tokens[], bool asLValue, DATATYPE & datatype);
    void ParsePrimary(TOKEN tokens[], DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    EnterModule("Prefix");

    if ((tokens[0].type == INC) ||
        (tokens[0].type == DEC)
        )
    {
        DATATYPE datatypeRHS;
        TOKENTYPE operation = tokens[0].type;

        GetNextToken(tokens);
        ParseVariable(tokens, true, datatypeRHS);

        if (datatypeRHS != INTEGERTYPE)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting integer operand");

        switch (operation)
        {
        case INC:
            code.EmitFormattedLine("", "PUSH", "@SP:0D0");
            code.EmitFormattedLine("", "PUSH", "#0D1");
            code.EmitFormattedLine("", "ADDI");
            code.EmitFormattedLine("", "POP", "@SP:0D1");       // side-effect
            code.EmitFormattedLine("", "PUSH", "@SP:0D0");
            code.EmitFormattedLine("", "SWAP");
            code.EmitFormattedLine("", "DISCARD", "#0D1");      // value
            break;
        case DEC:
            code.EmitFormattedLine("", "PUSH", "@SP:0D0");
            code.EmitFormattedLine("", "PUSH", "#0D1");
            code.EmitFormattedLine("", "SUBI");
            code.EmitFormattedLine("", "POP", "@SP:0D1");       // side-effect
            code.EmitFormattedLine("", "PUSH", "@SP:0D0");
            code.EmitFormattedLine("", "SWAP");
            code.EmitFormattedLine("", "DISCARD", "#0D1");      // value
            break;
        }
        datatype = INTEGERTYPE;
    }
    else
        ParsePrimary(tokens, datatype);

    ExitModule("Prefix");
}

//-----------------------------------------------------------
void ParsePrimary(TOKEN tokens[], DATATYPE& datatype) /*** updated ***/
//-----------------------------------------------------------
{
    void ParseVariable(TOKEN tokens[], bool asLValue, DATATYPE & datatype);
    void ParseExpression(TOKEN tokens[], DATATYPE & datatype);
    void ParseAssociativeArrayReference(TOKEN tokens[], bool asLvalue, DATATYPE & datatype);
    void GetNextToken(TOKEN tokens[]);

    EnterModule("Primary");

    char operand[SOURCELINELENGTH + 1]; 

    bool isInTable;
    int index;

    switch (tokens[0].type)
    {
    case INTEGER:
        sprintf(operand, "#0D%s", tokens[0].lexeme);
        code.EmitFormattedLine("", "PUSH", operand);
        datatype = INTEGERTYPE;
        GetNextToken(tokens);
        break;
    case TRUE:
        code.EmitFormattedLine("", "PUSH", "#0XFFFF");
        datatype = BOOLEANTYPE;
        GetNextToken(tokens);
        break;
    case FALSE:
        code.EmitFormattedLine("", "PUSH", "#0X0000");
        datatype = BOOLEANTYPE;
        GetNextToken(tokens);
        break;
    case CHARACTER:
        sprintf(operand, "#%s", tokens[0].lexeme);
        code.EmitFormattedLine("", "PUSH", operand);
        datatype = CHARACTERTYPE;
        GetNextToken(tokens);
        break;
    case FLOATVAL:
        sprintf(operand, "#0F%s", tokens[0].lexeme);
        code.EmitFormattedLine("", "PUSH", operand);
        datatype = FLOATTYPE;
        GetNextToken(tokens);
        break;
    case L_PAREN:
        GetNextToken(tokens);
        ParseExpression(tokens, datatype);
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting )");
        GetNextToken(tokens);
        break;
    // addr(<variable>)
    case ADDR:

        GetNextToken(tokens);
        
        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting (");
        GetNextToken(tokens);

        if (tokens[0].type != IDENTIFIER)
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");
        }

        index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
        if (!isInTable)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Undefined identifier");

        //==========================
        // MUST be a variable reference
        //==========================
        if (identifierTable.GetType(index) == FUNCTION_SUBPROGRAMMODULE || identifierTable.GetDatatype(index) == ASSOCTYPE)
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Identifier must refer to a variable");
        }

        // Get the datatype of the referenced variable, and log it as the 
        //     datatype of the CONTENTS OF the pointer(s)
        DATATYPE contentDatatype;
        
        contentDatatype = identifierTable.GetDatatype(index);

        for (int ptr : ptr_indexes)
        {
            identifierTable.SetContentDatatype(ptr, contentDatatype);
        }

        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting )");
        GetNextToken(tokens);

        // set the datatype to POINTERTYPE
        datatype = POINTERTYPE;

     // CODEGENERATION
        // push the ADDRESS OF the variable
        code.EmitFormattedLine("", "PUSHA", identifierTable.GetReference(index));
     // END CODEGENERATION

        break;

    // cont(<ptr variable>)
    case CONT:

        GetNextToken(tokens);

        // Check for left parenthesis
        if (tokens[0].type != L_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting (");
        GetNextToken(tokens);

        if (tokens[0].type != IDENTIFIER)
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting identifier");
        }

        index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
        if (!isInTable)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Undefined identifier");

        //==========================
        // MUST be a pointer reference
        //==========================
        if (identifierTable.GetDatatype(index) != POINTERTYPE )
        {
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Identifier must refer to a pointer variable.");
        }

        GetNextToken(tokens);

        // Check for right parenthesis
        if (tokens[0].type != R_PAREN)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting )");
        GetNextToken(tokens);

        // Get the datatype of the contents of the pointer
        datatype = identifierTable.GetContentDatatype(index);


        // CODEGENERATION
        sprintf(operand, "@%s", identifierTable.GetReference(index));
        code.EmitFormattedLine("", "PUSH", operand);
        // ENDCODEGENERATION

        break;

//  <AssociativeArrayReference> | <variable> | <enumConstant> | <FunctionReference>  
    case IDENTIFIER:
    {
        bool isInTable;
        int index;
        
        // Variables used to check if the identifier is an enumConstant
        bool isEnumConstant = false;
        vector<string> v;
        vector<string>::iterator it;

        // see if the identifier can be found in the enumMap
        if (lValueEnumType != "")
        {
            v = enumMap[lValueEnumScope][lValueEnumType];
            it = find(v.begin(), v.end(), tokens[0].lexeme);

            // if found, set isEnumConstant to true
            if (it != v.end())
            {
                isEnumConstant = true;
            }

            // Check if the enum type of the l-value can be found in the global scope
            else if (lValueEnumScope == PROGRAMMODULESCOPE || lValueEnumScope == SUBPROGRAMMODULESCOPE)
            {
                if (enumMap[GLOBALSCOPE].find(lValueEnumType) != enumMap[GLOBALSCOPE].end())
                {
                    v = enumMap[GLOBALSCOPE][lValueEnumType];
                    it = find(v.begin(), v.end(), tokens[0].lexeme);

                    // if found, set isEnumConstant to true
                    if (it != v.end())
                    {
                        isEnumConstant = true;
                    }
                }
            }
        }
        
        // If identifier is undefined if it's not in the identifier table AND it's not an enum constant
        //   (enum constants are not stored in the identifier table)
        index = identifierTable.GetIndex(tokens[0].lexeme, isInTable);
        if (!(isInTable || isEnumConstant))
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Undefined identifier");
        
        if (identifierTable.GetType(index) != FUNCTION_SUBPROGRAMMODULE)
        {

        //==========================
        // Associative array reference
        //==========================
            if (identifierTable.GetDatatype(index) == ASSOCTYPE)
            {

                ParseAssociativeArrayReference(tokens, false, datatype);

                // CODEGENERATION
                /*** Get the value for the given key in the associative array ***/
                code.EmitFormattedLine("", "GETAAE", identifierTable.GetReference(index));
                // ENDCODEGENERATION
            }
     
        //==========================
        // Enumeration constant
        //==========================
            else if (isEnumConstant)
            {
                // Convert the enumConstant to an integer by getting its index in its enumMap vector 
                int enumToInt = it - v.begin();

                sprintf(operand, "#0D%d", enumToInt);
                code.EmitFormattedLine("", "PUSH", operand);

                datatype = ENUMTYPE;
                GetNextToken(tokens);

            }

        //==========================
        // variable reference
        //==========================
            else
            {
            
                ParseVariable(tokens, false, datatype);

                // When assigning a pointer to another pointer, set the contents 
                //   datatype of the l-value pointer to that of the r-value pointer.
                if (datatype == POINTERTYPE)
                {
                    contentDatatype = identifierTable.GetContentDatatype(index);

                    for (int ptr : ptr_indexes)
                    {
                        identifierTable.SetContentDatatype(ptr, contentDatatype);
                    }
                }
            }
        }

        //==========================
        // FUNCTION_SUBPROGRAMMODULE reference
        //==========================
        else
        {
            char operand[MAXIMUMLENGTHIDENTIFIER + 1];
            int parameters;

            GetNextToken(tokens);
            if (tokens[0].type != L_PAREN)
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '('");

            // CODEGENERATION
            code.EmitFormattedLine("", "PUSH", "#0X0000", "reserve space for function return value");
            // ENDCODEGENERATION

            datatype = identifierTable.GetDatatype(index);
            parameters = 0;
            if (tokens[1].type == R_PAREN)
            {
                GetNextToken(tokens);
            }
            else
            {
                do
                {
                    DATATYPE expressionDatatype;

                    GetNextToken(tokens);
                    ParseExpression(tokens, expressionDatatype);
                    parameters++;

                    // STATICSEMANTICS
                    if (expressionDatatype != identifierTable.GetDatatype(index + parameters))
                        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                            "Actual parameter data type does not match formal parameter data type");
                    // ENDSTATICSEMANTICS

                } while (tokens[0].type == COMMA);
            }

            
            // STATICSEMANTICS
            if (identifierTable.GetCountOfFormalParameters(index) != parameters)
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
                    "Number of actual parameters does not match number of formal parameters");
            // ENDSTATICSEMANTICS

            if (tokens[0].type != R_PAREN)
                ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ')'");
            GetNextToken(tokens);

            // CODEGENERATION
            code.EmitFormattedLine("", "PUSHFB");
            code.EmitFormattedLine("", "CALL", identifierTable.GetReference(index));
            code.EmitFormattedLine("", "POPFB");
            sprintf(operand, "#0D%d", parameters);
            code.EmitFormattedLine("", "DISCARD", operand);
            // ENDCODEGENERATION
        }
    }
        break;
    default:
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, 
            "Expecting literal, '(', variable, associative array reference, or FUNCTION ('func') identifier");
        break;
    }

    ExitModule("Primary");
}

//-----------------------------------------------------------
void ParseVariable(TOKEN tokens[],bool asLValue,DATATYPE &datatype) /*** updated ***/
//-----------------------------------------------------------
{
/*
Syntax "locations"                 l- or r-value
---------------------------------  -------------
<expression>                       r-value
<prefix>                           l-value
<InputStatement>                   l-value
LHS of <assignmentStatement>       l-value
<ForStatement>                     l-value
OUT <formalParameter>              l-value
IO <formalParameter>               l-value
REF <formalParameter>              l-value

r-value ( read-only): value is pushed on run-time stack
l-value (read/write): address of value is pushed on run-time stack
*/
   void GetNextToken(TOKEN tokens[]);

   bool isInTable;
   int index;
   int dimensions;
   IDENTIFIERTYPE identifierType;

   EnterModule("Variable");

   if ( tokens[0].type != IDENTIFIER )
      ProcessCompilerError(tokens[0].sourceLineNumber,tokens[0].sourceLineIndex,"Expecting identifier");

// STATICSEMANTICS
   index = identifierTable.GetIndex(tokens[0].lexeme,isInTable);
   if ( !isInTable )
      ProcessCompilerError(tokens[0].sourceLineNumber,tokens[0].sourceLineIndex,"Undefined identifier");
   
   identifierType = identifierTable.GetType(index);
   datatype = identifierTable.GetDatatype(index);

   // Add the indexes of pointer l-values to the ptr_indexes vector
   if (datatype == POINTERTYPE)
   {
       if (asLValue)
       {
           // Add to pointer vector           
           ptr_indexes.push_back(index);
       }
       
   }

   if (!((identifierType == GLOBAL_VARIABLE) ||
       (identifierType == GLOBAL_CONSTANT) ||
       (identifierType == PROGRAMMODULE_VARIABLE) ||
       (identifierType == PROGRAMMODULE_CONSTANT) ||
       (identifierType == SUBPROGRAMMODULE_VARIABLE) ||
       (identifierType == SUBPROGRAMMODULE_CONSTANT) ||
       (identifierType == IN_PARAMETER) ||
       (identifierType == OUT_PARAMETER) ||
       (identifierType == IO_PARAMETER) ||
       (identifierType == REF_PARAMETER)))
      ProcessCompilerError(tokens[0].sourceLineNumber,tokens[0].sourceLineIndex,"Expecting variable or constant identifier");
      
   if (asLValue && ((identifierType == GLOBAL_CONSTANT) ||
       (identifierType == PROGRAMMODULE_CONSTANT) ||
       (identifierType == SUBPROGRAMMODULE_CONSTANT)))
       ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Constant may not be l-value");

   if (asLValue && (identifierType == GLOBAL_VARIABLE) && code.IsInModuleBody(FUNCTION_SUBPROGRAMMODULE))
       ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "FUNCTION ('func') may not modify global variable");
// ENDSTATICSEMANTICS

// CODEGENERATION
   if (identifierTable.GetDimensions(index) == 0)
   {
       if (asLValue)
           code.EmitFormattedLine("", "PUSHA", identifierTable.GetReference(index));
       else
           code.EmitFormattedLine("", "PUSH", identifierTable.GetReference(index));
   }
   else
   {
       GetNextToken(tokens);
       if (tokens[0].type != L_BRACKET)
           ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '['");
       dimensions = 0;
       do
       {
           DATATYPE expressionDatatype;

           GetNextToken(tokens);
           ParseExpression(tokens, expressionDatatype);
           dimensions++;
           if (expressionDatatype != INTEGERTYPE)
               ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Index expression must be integer");
       } while (tokens[0].type == COMMA);

       if (identifierTable.GetDimensions(index) != dimensions)
           ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
               "Number of index expressions does not match array dimensions");

       if (tokens[0].type != R_BRACKET)
           ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting ']'");

       if (asLValue)
           code.EmitFormattedLine("", "ADRAE", identifierTable.GetReference(index));
       else
           code.EmitFormattedLine("", "GETAE", identifierTable.GetReference(index));
   }
   
// ENDCODEGENERATION

   GetNextToken(tokens);

   ExitModule("Variable");
}


//-----------------------------------------------------------
void ParseAssociativeArrayReference(TOKEN tokens[], bool asLvalue, DATATYPE &datatype)   /*** new ***/
//-----------------------------------------------------------
{
    void GetNextToken(TOKEN tokens[]);
    DATATYPE keyDatatype;

    EnterModule("AssociativeArrayReference");

    // Get the next token
    GetNextToken(tokens);

    // Check for a left brace: '{'
    if (tokens[0].type != L_BRACE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '{'");
    GetNextToken(tokens);

    // Parse the expression between the braces
    ParseExpression(tokens, keyDatatype);

    // Check that the key is a scalar datatype
    if ((keyDatatype != INTEGERTYPE) && (keyDatatype != FLOATTYPE) &&
        (keyDatatype != CHARACTERTYPE) && (keyDatatype != BOOLEANTYPE))
    {
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex,
            "Key must be of a scalar datatype (int, bool, char, or float)");
    }

    // Check for a right brace: '}'
    if (tokens[0].type != R_BRACE)
        ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting '}'");
    
    // If the reference is an r-value, use type-casting operator 'as' to cast the value
    //    to one of the four scalar datatypes (This must be done since STM does not offer 
    //    run-time type checking.)
    if (!asLvalue)
    {
        GetNextToken(tokens);
        if (tokens[0].type != AS)
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting type-cast operator 'as'");

        GetNextToken(tokens);
        switch (tokens[0].type)
        {
        case INT:
            datatype = INTEGERTYPE;
            break;
        case BOOL:
            datatype = BOOLEANTYPE;
            break;
        case CHAR:
            datatype = CHARACTERTYPE;
            break;
        case FLOAT:
            datatype = FLOATTYPE;
            break;
        default:
            ProcessCompilerError(tokens[0].sourceLineNumber, tokens[0].sourceLineIndex, "Expecting 'int', 'bool', 'char', or 'float'");
            break;
        }

    }
   
    // End of module
    GetNextToken(tokens);

    ExitModule("AssociativeArrayReference");

}


//-----------------------------------------------------------
void Callback1(int sourceLineNumber,const char sourceLine[])
//-----------------------------------------------------------
{
   cout << setw(4) << sourceLineNumber << " " << sourceLine << endl;
}

//-----------------------------------------------------------
void Callback2(int sourceLineNumber,const char sourceLine[])
//-----------------------------------------------------------
{
   char line[SOURCELINELENGTH+1];

// CODEGENERATION
   sprintf(line,"; %4d %s",sourceLineNumber,sourceLine);
   code.EmitUnformattedLine(line);
// ENDCODEGENERATION
}

//-----------------------------------------------------------
void GetNextToken(TOKEN tokens[]) 
//-----------------------------------------------------------
{
    const char* TokenDescription(TOKENTYPE type);

    int i;
    TOKENTYPE type;
    char lexeme[SOURCELINELENGTH + 1];
    int sourceLineNumber;
    int sourceLineIndex;
    char information[SOURCELINELENGTH + 1];

    //============================================================
    // Move look-ahead "window" to make room for next token-and-lexeme
    //============================================================
    for (int i = 1; i <= LOOKAHEAD; i++)
        tokens[i - 1] = tokens[i];

    char nextCharacter = reader.GetLookAheadCharacter(0).character;

    //============================================================
    // "Eat" white space and comments
    //============================================================
    do
    {
        //    "Eat" any white-space (blanks and EOLCs and TABCs) 
        while ((nextCharacter == ' ')
            || (nextCharacter == READER<CALLBACKSUSED>::EOLC)
            || (nextCharacter == READER<CALLBACKSUSED>::TABC))
            nextCharacter = reader.GetNextCharacter().character;

        //    "Eat" line comment
        if ((nextCharacter == '$') && (reader.GetLookAheadCharacter(1).character == '$'))
        {

#ifdef TRACESCANNER
            sprintf(information, "At (%4d:%3d) begin line comment",
                reader.GetLookAheadCharacter(0).sourceLineNumber,
                reader.GetLookAheadCharacter(0).sourceLineIndex);
            lister.ListInformationLine(information);
#endif

            do
                nextCharacter = reader.GetNextCharacter().character;
            while (nextCharacter != READER<CALLBACKSUSED>::EOLC);
        }

        //    "Eat" block comments (nesting allowed)
        if ((nextCharacter == '$') && (reader.GetLookAheadCharacter(1).character == '*'))
        {
            int depth = 0;

            do
            {
                if ((nextCharacter == '$') && (reader.GetLookAheadCharacter(1).character == '*'))
                {
                    depth++;

#ifdef TRACESCANNER
                    sprintf(information, "At (%4d:%3d) begin block comment depth = %d",
                        reader.GetLookAheadCharacter(0).sourceLineNumber,
                        reader.GetLookAheadCharacter(0).sourceLineIndex,
                        depth);
                    lister.ListInformationLine(information);
#endif

                    nextCharacter = reader.GetNextCharacter().character;
                    nextCharacter = reader.GetNextCharacter().character;
                }
                else if ((nextCharacter == '*') && (reader.GetLookAheadCharacter(1).character == '$'))
                {

#ifdef TRACESCANNER
                    sprintf(information, "At (%4d:%3d)   end block comment depth = %d",
                        reader.GetLookAheadCharacter(0).sourceLineNumber,
                        reader.GetLookAheadCharacter(0).sourceLineIndex,
                        depth);
                    lister.ListInformationLine(information);
#endif

                    depth--;
                    nextCharacter = reader.GetNextCharacter().character;
                    nextCharacter = reader.GetNextCharacter().character;
                }
                else
                    nextCharacter = reader.GetNextCharacter().character;
            } while ((depth != 0) && (nextCharacter != READER<CALLBACKSUSED>::EOPC));
            if (depth != 0)
                ProcessCompilerError(reader.GetLookAheadCharacter(0).sourceLineNumber,
                    reader.GetLookAheadCharacter(0).sourceLineIndex,
                    "Unexpected end-of-program");
        }
    } while ((nextCharacter == ' ')
        || (nextCharacter == READER<CALLBACKSUSED>::EOLC)
        || (nextCharacter == READER<CALLBACKSUSED>::TABC)
        || ((nextCharacter == '$') && (reader.GetLookAheadCharacter(1).character == '$'))
        || ((nextCharacter == '$') && (reader.GetLookAheadCharacter(1).character == '*')));

    //============================================================
    // Scan token
    //============================================================
    sourceLineNumber = reader.GetLookAheadCharacter(0).sourceLineNumber;
    sourceLineIndex = reader.GetLookAheadCharacter(0).sourceLineIndex;

    // reserved words (and <identifier>)
    if (isalpha(nextCharacter))
    {
        char UCLexeme[SOURCELINELENGTH + 1];

        i = 0;
        lexeme[i++] = nextCharacter;
        nextCharacter = reader.GetNextCharacter().character;
        while (isalpha(nextCharacter) || isdigit(nextCharacter) || (nextCharacter == '_'))
        {
            lexeme[i++] = nextCharacter;
            nextCharacter = reader.GetNextCharacter().character;

        }
        lexeme[i] = '\0';
        for (i = 0; i <= (int)strlen(lexeme); i++)
            UCLexeme[i] = toupper(lexeme[i]);

        // check if lexeme is the keyword FILE
        if (strcmp(UCLexeme, "FILE") == 0)
        {
            type = FILE_T;
        }

        // check if lexeme is an instance of an END-token or an ELSE-token
        bool isEndToken = false;

        if (strcmp(UCLexeme, "END") == 0) {

            isEndToken = true;

            // search for a single space followed by the phrase "main"
            if (nextCharacter == ' ') {

                int secondPart = strlen(lexeme); //marking index of second part
                i = secondPart;
                lexeme[i++] = ' ';
                nextCharacter = reader.GetNextCharacter().character;

                while (isalpha(nextCharacter)) {
                    lexeme[i++] = nextCharacter;
                    nextCharacter = reader.GetNextCharacter().character;
                }
                lexeme[i] = '\0';

                for (i = secondPart; i <= (int)strlen(lexeme); i++)
                    UCLexeme[i] = toupper(lexeme[i]);

                if (strcmp(UCLexeme, "END MAIN") == 0) {
                    type = ENDMAIN;
                }
                else if (strcmp(UCLexeme, "END IF") == 0) {
                    type = ENDIF;
                }
                else if (strcmp(UCLexeme, "END WHILE") == 0) {
                    type = ENDWHILE;
                }
                else if (strcmp(UCLexeme, "END FOR") == 0) {
                    type = ENDFOR;
                }
                else if (strcmp(UCLexeme, "END PROC") == 0) {
                    type = ENDPROC;
                }
                else if (strcmp(UCLexeme, "END FUNC") == 0) {
                    type = ENDFUNC;
                }
            }
        }


        if (!isEndToken) {
            bool isFound = false;

            i = 0;
            while (!isFound && (i <= (sizeof(TOKENTABLE) / sizeof(TOKENTABLERECORD)) - 1))
            {
                if (TOKENTABLE[i].isReservedWord && (strcmp(UCLexeme, TOKENTABLE[i].description) == 0))
                    isFound = true;
                else
                    i++;
            }
            if (isFound)
                type = TOKENTABLE[i].type;
            else
                type = IDENTIFIER;
        }


    }
    // <integer> and <float>
    else if (isdigit(nextCharacter))
    {
        i = 0;
        lexeme[i++] = nextCharacter;
        nextCharacter = reader.GetNextCharacter().character;
        while (isdigit(nextCharacter))
        {
            lexeme[i++] = nextCharacter;
            nextCharacter = reader.GetNextCharacter().character;
        }
        // if floating point does not exist, set as <integer>
        if (nextCharacter != '.')
        {
            lexeme[i] = '\0';
            type = INTEGER;
        }
        // if floating point exists, set as <float>
        else
        {
            // Add nextCharacter to lexeme, increment index
            lexeme[i++] = nextCharacter;

            // Get next character
            nextCharacter = reader.GetNextCharacter().character;

            // Check if there is a digit after the floating point
            if (!isdigit(nextCharacter))
            {
                ProcessCompilerError(sourceLineNumber, sourceLineIndex,
                    "Digit must follow floating point");
            }
            else
            {
                while (isdigit(nextCharacter))
                {
                    // Add nextCharacter to lexeme, increment index
                    lexeme[i++] = nextCharacter;

                    // Get next character
                    nextCharacter = reader.GetNextCharacter().character;
                }

                // Check if 'e' is written after the decimal places
                if (nextCharacter == 'e')
                {
                    // Add nextCharacter to lexeme, increment index
                    lexeme[i++] = nextCharacter;

                    // Get next character
                    nextCharacter = reader.GetNextCharacter().character;

                    // Check if a '-' is after the 'e'
                    if (nextCharacter == '-')
                    {
                        // Add nextCharacter to lexeme, increment index
                        lexeme[i++] = nextCharacter;

                        // Get next character
                        nextCharacter = reader.GetNextCharacter().character;
                    }

                    // Check if there is an integer value after e
                    if (!isdigit(nextCharacter))
                    {
                        ProcessCompilerError(sourceLineNumber, sourceLineIndex,
                            "e must be followed by an integer value");
                    }
                    else
                    {
                        while (isdigit(nextCharacter))
                        {
                            // Add nextCharacter to lexeme, increment index
                            lexeme[i++] = nextCharacter;

                            // Get next character
                            nextCharacter = reader.GetNextCharacter().character;
                        }
                    }
                }
            }

            lexeme[i] = '\0';
            type = FLOATVAL;
        }
    }
    else
    {
        switch (nextCharacter)
        {
        // <character>
        case '\'':
            ;
         // Set index for lexeme to 0
            i = 0;
            lexeme[i++] = nextCharacter;
         // Get nextChar
            nextCharacter = reader.GetNextCharacter().character;

         // If nextChar is '
            if (nextCharacter == '\'')
            {
                // Give error: "Expecting ASCII character"
                ProcessCompilerError(sourceLineNumber, sourceLineIndex,
                    "Expecting ASCII character");
            }             
         // Else
            else
            { 
             // Add nextChar to lexeme, increment index
                lexeme[i++] = nextCharacter;

             // Get nextChar
                nextCharacter = reader.GetNextCharacter().character;

             // If nextChar != '
                if (nextCharacter != '\'')
                {
                    // Give error: "Invalid char literal"
                    ProcessCompilerError(sourceLineNumber, sourceLineIndex,
                        "Invalid char literal");
                }
             // Else
                else
                {
                    // Add nextChar to lexeme, increment index
                    lexeme[i++] = nextCharacter;

                    // Add null character to lexeme
                    lexeme[i] = '\0';

                    // Set type to CHARACTER
                    type = CHARACTER;

                    // Call reader.GetNextCharacter()
                    reader.GetNextCharacter();
                }
             }
         // break
            break;
        // <string>
        case '"':
            i = 0;
            nextCharacter = reader.GetNextCharacter().character;
            while ((nextCharacter != '"') && (nextCharacter != READER<CALLBACKSUSED>::EOLC))
            {
                if ((nextCharacter == '\\') && (reader.GetLookAheadCharacter(1).character == '"'))
                {
                    lexeme[i++] = nextCharacter;
                    nextCharacter = reader.GetNextCharacter().character;
                }
                else if ((nextCharacter == '\\') && (reader.GetLookAheadCharacter(1).character == '\\'))
                {
                    lexeme[i++] = nextCharacter;
                    nextCharacter = reader.GetNextCharacter().character;
                }
                lexeme[i++] = nextCharacter;
                nextCharacter = reader.GetNextCharacter().character;
            }
            if (nextCharacter == READER<CALLBACKSUSED>::EOLC)
                ProcessCompilerError(sourceLineNumber, sourceLineIndex,
                    "Invalid string literal");
            lexeme[i] = '\0';
            type = STRING;
            reader.GetNextCharacter();
            break;
        case READER<CALLBACKSUSED>::EOPC:
        {
            static int count = 0;

            if (++count > (LOOKAHEAD + 1))
                ProcessCompilerError(sourceLineNumber, sourceLineIndex,
                    "Unexpected end-of-program");
            else
            {
                type = EOPTOKEN;
                reader.GetNextCharacter();
                lexeme[0] = '\0';
            }
        }
        break;
        case ',':
            type = COMMA;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case ';':
            type = SEMICOLON;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case '(':
            type = L_PAREN;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case ')':
            type = R_PAREN;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case '[':
            type = L_BRACKET;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case ']':
            type = R_BRACKET;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case '{':
            type = L_BRACE;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case '}':
            type = R_BRACE;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case '<':
            lexeme[0] = nextCharacter;
            nextCharacter = reader.GetNextCharacter().character;
            if (nextCharacter == '=')
            {
                type = LTEQ;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                reader.GetNextCharacter();
            }
            else if (nextCharacter == '>')
            {
                type = NOTEQ;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                reader.GetNextCharacter();
            }
            else
            {
                type = LT;
                lexeme[1] = '\0';
            }
            break;
        // use character look-ahead to "find" other '='
        case '=':
            lexeme[0] = nextCharacter;
            if (reader.GetLookAheadCharacter(1).character == '=')
            {
                nextCharacter = reader.GetNextCharacter().character;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                reader.GetNextCharacter();
                type = EQ;
            }
            else // a single '=' is an assignment operator
            {
                type = ASSIGN;
                lexeme[1] = '\0';
                reader.GetNextCharacter();
            }
            break;
        case '>':
            lexeme[0] = nextCharacter;
            nextCharacter = reader.GetNextCharacter().character;
            if (nextCharacter == '=')
            {
                type = GTEQ;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                reader.GetNextCharacter();
            }
            else
            {
                type = GT;
                lexeme[1] = '\0';
            }
            break;
        // use character look-ahead to "find" '='
        case '!':
            lexeme[0] = nextCharacter;
            if (reader.GetLookAheadCharacter(1).character == '=')
            {
                nextCharacter = reader.GetNextCharacter().character;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                reader.GetNextCharacter();
                type = NOTEQ;
            }
            else
            {
                type = UNKTOKEN;
                lexeme[1] = '\0';
                reader.GetNextCharacter();
            }
            break;
        case '+':
            lexeme[0] = nextCharacter;
            if (reader.GetLookAheadCharacter(1).character == '+')
            {
                nextCharacter = reader.GetNextCharacter().character;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                type = INC;
            }
            else
            {
                type = PLUS;
                lexeme[0] = nextCharacter; lexeme[1] = '\0';
            }
            reader.GetNextCharacter();
            break;
        case '-':
            lexeme[0] = nextCharacter;
            if (reader.GetLookAheadCharacter(1).character == '-')
            {
                nextCharacter = reader.GetNextCharacter().character;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                type = DEC;
            }
            else
            {
                type = MINUS;
                lexeme[0] = nextCharacter; lexeme[1] = '\0';
            }
            reader.GetNextCharacter();
            break;
        // use character look-ahead to "find" other '*'
        case '*':
            lexeme[0] = nextCharacter;
            if (reader.GetLookAheadCharacter(1).character == '*')
            {
                nextCharacter = reader.GetNextCharacter().character;
                lexeme[1] = nextCharacter; lexeme[2] = '\0';
                type = POWER;
            }
            else
            {
                type = MULTIPLY;
                lexeme[0] = nextCharacter; lexeme[1] = '\0';
            }
            reader.GetNextCharacter();
            break;
        case '/':
            type = DIVIDE;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case '%':
            type = MODULUS;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case '^':
            type = POWER;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        case ':':
            type = COLON;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        default:
            type = UNKTOKEN;
            lexeme[0] = nextCharacter; lexeme[1] = '\0';
            reader.GetNextCharacter();
            break;
        }
    }

    tokens[LOOKAHEAD].type = type;
    strcpy(tokens[LOOKAHEAD].lexeme, lexeme);
    tokens[LOOKAHEAD].sourceLineNumber = sourceLineNumber;
    tokens[LOOKAHEAD].sourceLineIndex = sourceLineIndex;

#ifdef TRACESCANNER
    sprintf(information, "At (%4d:%3d) token = %12s lexeme = |%s|",
        tokens[LOOKAHEAD].sourceLineNumber,
        tokens[LOOKAHEAD].sourceLineIndex,
        TokenDescription(type), lexeme);
    lister.ListInformationLine(information);
#endif

}

//-----------------------------------------------------------
const char *TokenDescription(TOKENTYPE type)
//-----------------------------------------------------------
{
   int i;
   bool isFound;
   
   isFound = false;
   i = 0;
   while ( !isFound && (i <= (sizeof(TOKENTABLE)/sizeof(TOKENTABLERECORD))-1) )
   {
      if ( TOKENTABLE[i].type == type )
         isFound = true;
      else
         i++;
   }
   return ( isFound ? TOKENTABLE[i].description : "???????" );
}

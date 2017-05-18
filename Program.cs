using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace cs2javaZad1
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Program polega na obliczeniu pola i obwodu prostokąta");
            int bokA = 10;
            int bokB = 15;
            int obwod;
            obwod = (2 * bokA + 2 * bokB);
            int pole;
            pole = bokA * bokB;
            Console.WriteLine("Obwod wynosi: " + obwod + ", Pole wynosi: " + pole);
        }
    }
}

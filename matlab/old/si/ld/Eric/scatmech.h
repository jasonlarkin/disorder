//******************************************************************************
//** SCATMECH: Polarized Light Scattering C++ Class Library
//** 
//** File: scatmech.h
//**
//** Thomas A. Germer
//** Optical Technology Division, National Institute of Standards and Technology
//** 100 Bureau Dr. Stop 8443; Gaithersburg, MD 20899-8443
//** Phone: (301) 975-2876; FAX: (301) 975-6991
//** Email: thomas.germer@nist.gov
//**
//** Version: 6.00 (February 2008)
//**
//******************************************************************************
#ifndef SCATMECH_SCATMECH_H
#define SCATMECH_SCATMECH_H

#include <exception>  
#include <complex> 
#include <string>
#include "askuser.h"

namespace SCATMECH {

    typedef std::string STRING;
    typedef std::complex<double> COMPLEX;

    //
    // Some useful values...
    //
    const double pi = 4.*atan(1.);
    const double deg = pi/180.;

    const char tab   = '\t';
    const char space = ' ';
    const char ret   = '\r';

    //
    // Some useful inline functions...
    //
    template <class T>
    inline T sqr(const T& x) {return x*x;}
    template <class T>
    inline T cube(const T& x) {return x*x*x;}

    //
    // The following are useful for error handling...
    //

    class SCATMECH_exception : public std::exception
    {
    public:
   
        SCATMECH_exception(const std::string m)
        {
            message = "SCATMECH: ";
            message += std::string(m);
        }

        ~SCATMECH_exception() throw() {}

        virtual const char *what() const throw() 
        {
            return message.c_str();
        }

    private:
        std::string message;
    };

    template <class T>
    std::string to_string(const T& t)
    {
        std::ostringstream oss;
        oss << t;
        return oss.str();
    }

    template <class T>
    T from_string(const std::string& s)
    {
        T result;
        std::istringstream iss(s);
        iss >> result;
        //if (iss.fail()) throw SCATMECH_exception("Unrecognized value in from_string<T>()");
        return result;
    }

    template <>
    inline std::string from_string<std::string>(const std::string& s) 
    {
        return s;
    }

    std::string Get_SCATMECH_Version();

} // namespace SCATMECH 


#endif


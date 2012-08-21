//******************************************************************************
//** SCATMECH: Polarized Light Scattering C++ Class Library
//** 
//** File: askuser.h
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
#ifndef SCATMECH_ASKUSER_H
#define SCATMECH_ASKUSER_H

#include <fstream>
#include <istream>
#include <ostream>
#include <sstream>
#include <string>
#include <stack>

namespace SCATMECH {
    
    extern char* SCATMECH_DIRECTORY;
    void check_SCATMECH_DIRECTORY();
    std::string find_file(const std::string& name);
    
    
    /// @brief An input streambuf with some added features
    ///
    /// The streambuf_with_comments class is a stream buffer that removes comments 
    /// (remainder of any line starting with a semicolon) and allows
    /// for redirection of the input (any <filename).
    ///
    class streambuf_with_comments : public std::streambuf {
    private:
        std::stack<std::streambuf*> istr;
        
    protected:
        void clear_comment();
        void pushfile();
        void popfile();
        int underflow();
        int uflow();
        int pbackfail(char c);
    public:
        streambuf_with_comments(std::streambuf *s);
        streambuf_with_comments& operator=(std::streambuf *s);
        virtual ~streambuf_with_comments();
        void unwind();
    };
    
    /// @brief An input stream with some added features
    /// 
    /// The istream_with_comments uses streambuf_with_comments to ignore 
    /// comments and allow for redirection.
    ///
    class istream_with_comments : public std::istream {
    public:
        typedef std::istream istream;
        istream_with_comments(std::streambuf* _buf) : buf(_buf), istream(&buf) {}
        
        istream_with_comments& operator=(std::istream& _str) {
            buf = _str.rdbuf();
            return *this;
        }
        void unwind() {
            buf.unwind();
        }
        
        /// Get an entire line and return as a string
        std::string getstrline();
        
        /// Get the next token, which may be surrounded by double quotes
        std::string getquoted();
        
    protected:
        streambuf_with_comments buf;
    };
    
    /// @brief An input file stream with some added features
    /// 
    /// The ifstream_with_comments uses streambuf_with_comments to ignore 
    /// comments and allow for redirection.
    ///
    class ifstream_with_comments : public istream_with_comments {
    public:
        ifstream_with_comments(const char* name) : fbuf(), istream_with_comments(&fbuf) { 
            std::string newname = find_file(name);
            open(newname.c_str()); 
        }
        ifstream_with_comments() : istream_with_comments(&fbuf) {}
        void open(const char* name) {
            fbuf.open(name,ios_base::in);
            if (!fbuf.is_open()) setstate(failbit);
        }
        void close() { fbuf.close(); }
    protected:
        std::filebuf fbuf;
    };
    
    /// The input stream used by the SCATMECH library
    extern istream_with_comments SCATMECH_input;
    
    /// The output stream used by the SCATMECH library
    extern std::ostream SCATMECH_output;
    
    /// @brief Template function to query user for a value
    /// 
    /// Queries the user for a value.  It sends the prompt to the SCATMECH output
    /// stream, followed by the default value in angle brackets <> and waits 
    /// for user input.  If something throws an exception, it asks again.
    template <class TYPE>
    TYPE AskUser(
    const std::string& prompt,  ///< The prompt
    const TYPE& deflt
    ) 
    {
        while (1) {
            try {
                SCATMECH_output << prompt << " <" << deflt << "> :";
                
                std::istringstream line(SCATMECH_input.getstrline());
                
                TYPE result;
                line >> result;
                
                if (line.fail()) result = deflt;
                
                return result;
            } 
            catch (std::exception& e) {
                SCATMECH_output << e.what() << std::endl;
                SCATMECH_input.unwind();
                if (SCATMECH_input.eof()) throw e;
            }
        }
    }
    
    /// Function to query the user for a string
    inline std::string AskUser(const std::string& query, const char* deflt)
    {
        return AskUser(query,std::string(deflt));
    }
    
    /// Set the standard SCATMECH input stream
    void set_istream(std::istream &is);
    
    /// Set the standard SCATMECH output stream
    void set_ostream(std::ostream &os);
    
    /// @brief Template function for C style formatting
    ///
    /// This function is useful for output streams when one wants 
    /// to use tradtional C-style printf formats.  For example:
    ///      cout << format("%16.12E",x) << endl;
    template <class TYPE>
        std::string format(const std::string& fmt, const TYPE& value) {
        char buffer[256];
        sprintf(buffer,fmt.c_str(),value);
        // snprintf(buffer,256,fmt.c_str(),value);
        return std::string(buffer);
    }
    
    /// Get a token from an input stream
    std::string get_token(std::istream &is,const char* delim);
    
} // namespace SCATMECH 


#endif


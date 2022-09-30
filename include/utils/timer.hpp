#ifndef COPYWRITE_TIMER_HPP
#define COPYWRITE_TIMER_HPP

#include <chrono>
#include <vector>

class Timer
{
public:
    static void start();

    static std::string yield( std::size_t id);
private:
    static std::vector<std::chrono::time_point<std::chrono::system_clock>> epochs;
};



#endif //COPYWRITE_TIMER_HPP

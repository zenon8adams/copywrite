#include <iostream>
#include <sstream>
#include <string>
#include "utils/timer.hpp"

void Timer::start()
{
    epochs.emplace_back( std::chrono::system_clock::now());
    std::clog <<"Started timer with ID: " << epochs.size() -1 <<'\n';
}

std::string Timer::yield( std::size_t id)
{
    auto time_value     = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::system_clock::now() - epochs[ id]).count();
    size_t minutes      = time_value / ( 1000 * 60),
            seconds      = ( time_value - minutes * 1000 * 60) / 1000,
            milliseconds = ( time_value - seconds * 1000 - minutes * 1000 * 60);
    std::stringstream out;
    if( minutes != 0)
    {
        out << minutes << " minute";
        if( minutes > 1)
            out << "s";
    }

    if( seconds != 0)
    {
        if( minutes)
            out << ", " << seconds << " second";
        else
            out << seconds << " second";
        if( seconds > 1)
            out << "s";
    }

    if( minutes != 0 || seconds != 0)
        out << ", " << milliseconds << " millisecond";
    if( milliseconds > 1)
        out << "s";

    return out.str();
}

std::vector<std::chrono::time_point<std::chrono::system_clock>> Timer::epochs;

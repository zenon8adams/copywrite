#ifndef COPYWRITE_TIMER_HPP
#define COPYWRITE_TIMER_HPP

#include <chrono>
#include <vector>

class Timer
{
public:
    static void start()
    {
        epochs.emplace_back( std::chrono::system_clock::now());
        std::clog <<"Started timer with ID: " << epochs.size() -1 <<'\n';
    }

    template<typename Tp = std::chrono::seconds>
    static std::size_t yield( std::size_t id)
    {
        return std::chrono::duration_cast<Tp>( std::chrono::system_clock::now() - epochs[ id]).count();
    }
private:
    static std::vector<std::chrono::time_point<std::chrono::system_clock>> epochs;
};

std::vector<std::chrono::time_point<std::chrono::system_clock>> Timer::epochs;


#endif //COPYWRITE_TIMER_HPP

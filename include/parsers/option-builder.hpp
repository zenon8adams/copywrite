#ifndef PUZZLER_OPTION_BUILDER_HPP
#define PUZZLER_OPTION_BUILDER_HPP

#include <cstring>
#include <functional>
#include <string_view>

class OptionBuilder
{
public:
	OptionBuilder(int ac, char **av)
		: argc( ac), argv( av)
	{
	}

	void build()
	{
		int i = 1;
		while( i < argc)
		{
			auto *current_option = argv[ i++];
			int hypen_count = 0;
			while( *current_option == '-' && hypen_count <= 2)
			{
				++current_option;
				++hypen_count;
			}
			if( hypen_count == 0 && mis_handler)
			{
				mis_handler( current_option, i);
				continue;
			}
			auto *equal_to_position = strchr( current_option, '=');
			char *key = current_option, *value{ nullptr};
			if( equal_to_position != nullptr)
			{
				key  = current_option;
				*equal_to_position = '\0';
				value = equal_to_position + 1;
			}
			auto matching = options.find( key);
			if( matching != options.cend())
			{
				auto count = matching->second;
				if( count > 0 && value == nullptr)
				{
					while( count-- && i < argc)
						matched_options[ key].emplace_back( argv[ i++]);
				}
				else
					matched_options[ key].emplace_back( value == nullptr ? key : value);
			}
			else
				mis_handler( current_option, i);
		}
	}

	std::vector<std::string_view> get( std::string_view key) const
	{
		auto match = matched_options.find( key);
		if( match == matched_options.cend())
		{
			auto o_key = option_pair.find( key);
			if( o_key != option_pair.cend())
				match = matched_options.find( o_key->second);
			if( match != matched_options.cend())
				return match->second;
			auto default_match = options_default.find( key);
			if( default_match != options_default.cend())
				return default_match->second;
			return {};
		}
		return match->second;
	}

	const char *asDefault( const char *key) const
	{
		auto result = get( key);
		return result.empty() ? nullptr : result.front().data();
	}

	auto asBool( const char *key) const
	{
		auto result = asDefault( key);
		return result != nullptr;
	}

	auto asInt( const char *key) const
	{
		auto result = asDefault( key);
		return result == nullptr ? 0 : strtol( result, nullptr, 10);
	}

	auto asFloat( const char *key) const
	{
		auto result = asDefault( key);
		return result == nullptr ? 0.f : strtof( result, nullptr);
	}

	void addMismatchConsumer( const std::function<void(const char *, int)>& handler)
	{
		mis_handler = handler;
	}

	OptionBuilder& addOption( const char *long_key, const char *short_key = nullptr,
							  const std::vector<std::string_view>& default_values = {}, int n_args = 0)
	{
		options[ long_key] = n_args;
		if( short_key != nullptr)
		{
			options[ short_key] = n_args;
			option_pair[ long_key] = short_key;
			option_pair[ short_key] = long_key;
			if( !default_values.empty())
				options_default[ short_key] = default_values;
		}
		if( !default_values.empty())
			options_default[ long_key] = default_values;

		return *this;
	}

	OptionBuilder& addOption( const char *long_key, const char *short_key,
							  std::string_view default_value, int n_args = 0)
	{
		return addOption( long_key, short_key, std::vector<std::string_view>{ default_value}, n_args);
	}

	std::unordered_map<std::string_view, size_t> options;
	std::unordered_map<std::string_view, std::string_view> option_pair;
	std::unordered_map<std::string_view, std::vector<std::string_view>> matched_options;
	std::unordered_map<std::string_view, std::vector<std::string_view>> options_default;
	char **argv;
	int argc;
	std::function<void(const char *, int)> mis_handler;
};
#endif //PUZZLER_OPTION_BUILDER_HPP

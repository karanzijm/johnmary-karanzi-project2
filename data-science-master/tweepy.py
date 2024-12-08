import tweepy
import json
import time
# Replace with your credentials
API_KEY = "JgJshvAMsAvD01qWzlHGtk0kf"
API_SECRET_KEY = "KZUmSSlvRdGw3KumxGNroXN2CRj5u2swVQb0wgVjkwO2SQUYMN"
ACCESS_TOKEN = "1660254068692901889-yaGp2MlBeoSArrn2KWRiUDtofutqjj"
ACCESS_TOKEN_SECRET = "SMsZctAKpznvCHqNqDtUI5vJ3F0XhbCHzhhxLTOI8eCM6"
BEARER_TOKEN = "AAAAAAAAAAAAAAAAAAAAABm3xQEAAAAApzU6rky6NAiEfvFFu624DyJ%2B4Mw%3Drd2okl06VLCONYYobdGKGOCYqz9RnUB7fvgBx1cJRdBuEFZ7Cg"

# Authenticate
client = tweepy.Client(bearer_token=BEARER_TOKEN)

# Search recent tweets (last 7 days)
query = "Uganda"  # Replace with your search query
tweet_fields = [
    "id",          # Tweet ID
    "text",        # Tweet text
    "created_at",  # Creation time
    "author_id",   # Author's user ID
    "lang",        # Language
    "geo",         # Geographical location data
    "source",      # Source of the tweet (e.g., Twitter app used)
    "public_metrics",  # Engagement metrics (e.g., retweets, likes)
    "referenced_tweets",  # Replies or quotes
    "possibly_sensitive"  # Sensitivity flag
]

# Fetch tweets with rate-limiting handling
try:
    tweets = client.search_recent_tweets(query=query, max_results=50, tweet_fields=tweet_fields)  # Max is 100 per request

    # Save tweets to a JSON file
    if tweets.data:
        with open("tweets.json", "w") as file:
            json.dump([tweet.data for tweet in tweets.data], file, indent=4)
        print("Tweets saved to tweets.json")
    else:
        print("No tweets found for the given query.")

except tweepy.TooManyRequests:
    print("Rate limit reached. Waiting for 15 minutes...")
    time.sleep(15 * 60)  # Wait 15 minutes before retrying
    try:
        tweets = client.search_recent_tweets(query=query, max_results=50, tweet_fields=tweet_fields)
        if tweets.data:
            with open("tweets.json", "w") as file:
                json.dump([tweet.data for tweet in tweets.data], file, indent=4)
            print("Tweets saved to tweets.json after retry.")
        else:
            print("No tweets found for the given query after retry.")
    except Exception as retry_error:
        print("Error during retry:", retry_error)

except Exception as e:
    print("An error occurred:", e)

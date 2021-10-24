source(here::here("R", "utilities.R"))

# Load merged data + main platforms ============================================
load(here("data", "tidy", "cong_merged_donations.Rda"))

minor_list <- c(
  "actblue", "winred", "anedot", "authorize",
  "clickandpledge", "donorbox", "efundraising", "fundhero", 
  "ngp", "numero", "piryx", "raisethemoney", "transaxt"
) %>%
  set_names(., .) %>%
  map(~ loadRData(here("data", "tidy", paste0(.x, "_congress.Rda"))))

cong <- cong %>%
  imap(
    ~ anti_join(
      wayback_std(.x),
      Reduce(bind_rows, minor_list %>% map(.y)),
      by = "url"
    ) %>%
      filter(!grepl("paypal", url))
  )

## 11 and 50
cong %>% map_dbl(nrow)

# NGP VAN (but variation) ======================================================
minor_platforms(cong, "ngpvan")
ngpvan <- save_text_df("ngpvan")
ngpvan %>% map_dbl(nrow)
save(ngpvan, file = here("data", "tidy", "ngpvan_congress.Rda"))

cong$house <- anti_join(wayback_std(cong$house), ngpvan$house, by = "url")

# Senate manual ================================================================
senate <- cong$senate %>%
  select(url, link) %>%
  left_join(
    ., list(
      data.frame(
        link = "http://web.archive.org/web/20201107074556/https://www.allenrwaters.com/donate",
        title = "Donate to Allen Waters for Senate",
        text = "",
        footer = "Paid for The Waters Committee | Approved by Allen Waters P.O. Box 40565 - Providence, RI 02940 P (401) 200-5124 "
      ),
      data.frame(
        link = "http://web.archive.org/web/20201107064749/https://anguskingformaine.nationbuilder.com/",
        title = "Angus 2018 Independent for U.S. Senate",
        text = "As an all-too-rare independent in Congress, I strive to keep your interests at the heart of what I do and to avoid getting entangled in party politics. Without a D or an R next to my name, though, I rely on grassroots donors to help me keep my seat and continue my work in Congress. I’m now an “in-cycle” candidate, and I need your support more than ever. Can you make a contribution to help make sure I can keep working on important issues in the Senate?",
        footer = "If you'd prefer to contribute by check, please make payable to the Angus King for U.S. Senate Campaign, and mail to: Angus King for U.S. Senate Campaign PO Box 368, Brunswick, ME 04011 If you have any questions about your contribution or would like a refund, please contact info@angusformaine.com within 30 days. With other questions, contact us at: Angus King for U.S. Senate Campaign PO Box 368, Brunswick, ME 04011 info@angusformaine.com. Phone: 207-607-4298"
      ),
      data.frame(
        ## No title, text, nor footer
        link = "http://web.archive.org/web/20201018155614/https://cm.aristotle.com/d/roundsforsenate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201107060947/https://contribute.brianschatz.com/page/contribute/contribute",
        title = "Senator Brian Schatz: Make a contribution",
        text = "Your support is critical to giving our volunteers the resources to reach every voter to share Senator Schatz’s work to stand up for Hawai‘i's values and build an economy that works for all our families. Please fill out the form below to give.",
        footer = "Paid for by Schatz for Senate"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201031133733/https://www.dustinbmurphy.com/contribute",
        title = "Dustin Murphy for United States Senate",
        text = " Under the Federal Election Commission, individuals may contribute a maximum of $2,800 per election to a candidate's campaign. For contributions over $200 it is required to provide your employer and occupation. Since we're not a large campaign, we are asking that all contributions list their employer and occupation. We have also set a maximum contribution limit of $2,500 per individual. Thank you and God Bless! ",
        footer = "Committee to Elect Dustin Murphy"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201102202130/https://www.graysonforga.com/donate",
        title = "Donate",
        text = "If you prefer to use Paypal, click here: Pay via PayPal Mail Checks to: Grayson Committee PO BOX 1018 Lithonia, GA 30058",
        footer = "Grayson for Georgia"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201106035213/https://www.johnsonsenate.com/contribute-now",
        title = "Join Wayne's Mission To End Student Loan Debt",
        text = "1.7 Million Georgians Owe Student Loans. Join The Fight To Help Your Family and Friends. End Student Loan Debt Improve Georgia's Infrastructure - Roads, Waterways, Rail, Power Grid, Ports, Airports Fight For Lower Prescription Drug Prices and Comprehensive Elder Care Expand Georgia Military Bases Protect Our Borders from Unlawful Entry Promote Alternative Energy The Grassroots Fight To End Student Loan Debt Needs You. Invest Today!",
        footer = "Wayne Johnson for Senate, Inc. Macon, Georgia © Copyright 2020"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201203043851/https://mystique.victorypassport.com/pages/burr/post-election-donation-page?location=https%3A%2F%2Fgive.victorypassport.com%2Fburr%2Fcontribute",
        title = "Donate Today to Richard Burr!",
        text = "",
        footer = "By clicking Contribute I certify that the following statements are true and accurate: This contribution is made from my personal funds and is not drawn on an account maintained by a corporation, labor union, or national bank, and I am a U.S. citizen or lawfully admitted permanent resident and this contribution will not be reimbursed by another person. I agree to the Terms of Service Contributions are not tax deductible as charitable contributions for federal income tax purposes. Federal law requires us to obtain and report the name, mailing address, occupation and name of employer for each individual whose contributions aggregate in excess of $200 per election cycle. Contributions by corporations, foreign nationals (non-green card holders), labor unions, federal government contractors, and minors under the age of 18 are prohibited."
      ),
      data.frame(
        link = "http://web.archive.org/web/20201203032316/https://mystique.victorypassport.com/pages/murkowski/contribute?location=https%3A%2F%2Fgive.victorypassport.com%2Fmurkowski%2Fcontribute",
        title = "",
        text = "Freedom comes from strength and self-reliance. - Lisa Murkowski",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201030004514/https://secure.campaignsolutions.com/jimrisch/donation1/?initiativekey=N4MUBY3F3R6T",
        title = "Jim Risch U.S. Senate Donate Today",
        text = "Jim Risch earned his reputation as a popular “no-nonsense, get the job done leader” as Idaho’s Governor where he cut property taxes by 20%, bringing property tax relief to Idaho taxpayer. His foresight also provided $100 million in a rainy day fund for public schools that was desperately needed during the economic downturn.",
        footer = "Paid for by Jim Risch for U.S. Senate Committee - 407 W. Jefferson, Boise, Idaho 83702"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201107072330/https://secure.chuckschumer.com/",
        title = "Schumer U.S. Senate Make a Contribution",
        text = "Your contribution will give Chuck Schumer the opportunity to continue serving New Yorkers -- and all Americans -- in the U.S. Senate.",
        footer = "Paid for by Friends of Schumer"
      )
    ) %>%
      bind_rows()
  )

# House manual =================================================================
house <- cong$house %>%
  select(url, link) %>%
  left_join(
    ., list(
      data.frame(
        link = "http://web.archive.org/web/20201111181018/https://67320.campaignpartner.net/contribute",
        title = "Make a Difference! Donate Now.",
        text = "To make a donation Please complete the form below or mail your contribution to Patrick Delices For Congress 948 Longwood Avenue Bronx, NYS 10459. For contribution limits and requirements, please click here Thank you! Sincerely, Patrick Delices",
        footer = "Paid for by Patrick Delices For U.S. Congress"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201105050630/https://beeler4congress.nationbuilder.com/donate",
        title = "Beeler for Congress",
        text = "",
        footer = "Follow @Beeler4Congress on Twitter"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201108222819/https://www.campaigncontribution.com/payment/contribution/info/29f7be14-f873-419c-bf56-7f5668bc41ff",
        title = "Eddie Bernice Johnson United States Congress",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112201614/https://www.campaigncontribution.com/payment/contribution/info/57fe9544-1eb0-4626-bbf2-786b57def75f",
        title = "Emanuel Cleaver, II U.S. Congress",
        text = "",
        footer = "Paid for by Cleaver for Congress, Donna M. Wilson, Treasurer"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112181149/https://www.campaigncontribution.com/payment/contribution/info/6be33664-4500-45dd-a593-b2a47d017538",
        title = "",
        text = "",
        footer = "Copyright 2018 - Paid for by the Friends of Chris Smith"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201029095733/https://carlosforny.com/donation/",
        title = "Donate",
        text = 'Please donate right now to give our campaign the resources we need to take on Congresswoman Carolyn B. Maloney. All online donations are processed and powered by "STRIPE". Stripe is the most trusted and industry-leading payment gateway system which is the highest grade of payment processing security. You can rest assured that your information is safe and secure.',
        footer = "PAID FOR BY CARLOS SANTIAGO-CANO FOR CONGRESS"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201109035506/https://www.chuckforcongress.com/donate",
        title = "Donate",
        text = "Federal law limits contributions to Chuck Fleischmann for Congress, Inc. to $2,700 per election (primary and general 2018) from individuals, and $5,000 per election (primary and general) from qualified federal PACs. Federal law requires us to use our best efforts to collect and report the name, address, occupation, and name of employer of each individual whose contributions exceed $200 in an election cycle. Contributions to Chuck Fleischmann for Congress are not tax deductible for federal income tax purposes. Contributions by corporations, foreign nationals, and federal government contractors are prohibited.",
        footer = "PAID FOR BY CHUCK FLEISCHMANN FOR CONGRESS COMMITTEE, INC., RANDALL HEBERT, TREASURER"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201030071543/https://www.cooperforcongress.com/form/-8877775747779720448",
        title = "Contribute to Our Campaign",
        text = "Running for office is becoming a more and more expensive challenge. It takes the help of all my friends and  supporters to get our message out to interested Tennesseans. Chip in today to support our campaign and keep Middle Tennessee's voice strong.",
        footer = "Paid for by Cooper For Congress Committee, Robert A. Davidson, Treasurer. Contributions are not deductible for federal income tax purposes. Corporate and cash contributions prohibited."
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101033434/http://www.erikastottspearson.com/donate",
        title = "Donate to Erika Stotts Pearson for Congress",
        text = "Erika is fighting for a Tennessee where everyone has the opportunity to thrive! Education, Health Care, and the Economy are her top priorities and she is ready to bring that fight to Washington. Your generous contribution helps her get there!",
        footer = "PAID FOR BY ERIKA STOTTS PEARSON FOR CONGRESS"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201024171436/https://www.garamendi.org/contribute",
        title = "Support Congressman John Garamendi",
        text = "If you would like to contribute by check, please mail your contribution to P.O. Box 440, Walnut Grove, CA 95690",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005061620/https://garretgraves.ichooseapex.com/donate/",
        ## Not captured well; cookie?
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101082248/https://jim2020.com/donate/",
        title = "DONATE",
        text = "",
        footer = "Paid for by Jim 2020 Committee"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201104042810/https://www.jimcosta.com/donate",
        title = "Make A Contribution To Jim Costa For Congress",
        text = "CONTRIBUTE Keep Our Valley Strong",
        footer = "Paid for by Jim Costa for Congress"
      ),
      data.frame(
        ## Not scraped
        link = "http://web.archive.org/web/20201108075605/https://www.johnnyteague.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201114075103/https://mariodiazbalart.org/donate/",
        title = "Your donation makes a big difference",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113162231/http://www.markreedforcongress.com/donate/",
        title = "Contribute to the Campaign",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101003436/https://maryottforcongress.com/contribute",
        title = "YOUR DONATION TODAY HAS AN IMMEDIATE IMPACT ON OUR DISTRICT",
        text = "“Let’s make a difference for our communities; let’s enact solutions that we can be proud of.” — BRIAN MARYOTT",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201111181133/https://www.maureen4congress.com/donate/",
        title = "Donation Form",
        text = "",
        footer = "Vote for Maureen in 2020!"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201026124354/https://michaelguest.ms/donate/",
        title = "Donate",
        text = "",
        footer = "Friends of Michael Guest P. O. Box 470 Brandon, MS 39043"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201109064844/https://www.mikekellyforcongress.com/contribute/",
        title = "Contribute",
        text = "",
        footer = "Copyright © 2015 All Rights Reserved. Paid for by Mike Kelly for Congress. Design by Starn Marketing Group"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201106010248/https://www.mikethompsonforcongress.com/donate",
        title = "Contribute to Mike",
        text = "",
        footer = "Authorized & Paid For By Mike Thompson For Congress"
      ),
      data.frame(
        ## Nothing
        link = "http://web.archive.org/web/20201005053153/https://mystique.victorypassport.com/pages/baird/baird-donations?location=https%3A%2F%2Fgive.victorypassport.com%2Fbaird%2Fdonate&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vYmFpcmQvZG9uYXRlLyM%3D",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005102621/https://mystique.victorypassport.com/pages/joyce/main-donation-page?location=https%3A%2F%2Fgive.victorypassport.com%2Fjoyce%2Fdonate-today&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vam95Y2UvZG9uYXRlLXRvZGF5LyM%3D",
        title = "Donate Today to Help Propel Dr. John Joyce to Congress!",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113032152/https://www.parrott2020.org/donate-page",
        title = "Thank Your for your help.  Your Support Today Makes a Huge Difference.",
        text = "Please know that I will always use your donations wisely and with great care. Every amount helps.",
        footer = "PAID FOR BY NEIL PARROTT FOR CONGRESS"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113162754/https://petel4congress.com/donations/donation-form",
        title = "Donation form",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112204729/https://www.rabornforcongress.com/donate",
        title = "Donate",
        text = "Your contributions will go directly toward campaign expenses to deliver Theresa's message of hope, freedom, liberty, and equality to the entire district.  No one on the campaign takes a salary.  We are all working hard to represent you and fight the establishment.  Your support is greatly appreciated and Theresa is deeply humbled by your support!",
        footer = "Citizens for Theresa Raborn"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201105084414/https://reygonzalez.org/donate-today.html",
        title = "Rey Gonzalez for Congress",
        text = "",
        footer = "Paid for by the Rey Gonzalez for Congress Campaign, Mario Marinez, Treasurer, 109 E. Arroyo St. Harlingen, TX. 78550"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201106011308/https://rishikumar.com/contribute",
        title = "",
        text = "Contribute to a New Silicon Valley Vision. A New Energy, New Leadership. Rishi has pledged to NOT accept any PAC or Special Interest Group money. America deserves New Leadership, New Energy to solve our big challenges of today. An American Dream: A graduate student immigrant, now running for Congress.",
        footer = "Join the Grassroots Movement"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201105043954/https://www.rogersforwisconsin.com/donate",
        ## Nothing
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201027122520/https://schakowsky.bsd.net/page/contribute/default?donate_page_KEY=15",
        title = "MAKE AN ONLINE CONTRIBUTION",
        text = "Your contribution will help us in our campaign. We can't do it without you.",
        footer = "Paid for and Authorized by Schakowsky for Congress"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113162050/https://secure.campaignsolutions.com/calvert/list/proc/donation1/?InitiativeKey=LL39MZ8HTQXD",
        title = "Contribute Today",
        text = "Thank you for standing with me! Please consider making a contribution to help me continue the fight for conservative principles. - Ken",
        footer = "Paid for by Ken Calvert for Congress"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101182721/https://secure.campaignsolutions.com/kenbuck/list/proc/donation1/?initiativekey=V0T9TNVLFGX5",
        title = "Donate Today",
        text = "Ken Buck represents the people of the 4th District of Colorado. A former Weld Country District Attorney, business executive, and prosecutor with the U.S. Department of Justice, Ken is committed to representing the people with Conservative Leadership and an Independent Voice.",
        footer = "Paid for by Buck for Colorado"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201115010830/https://secure.campaignsolutions.com/webster/donation1/default.aspx?InitiativeKey=8LI7KDO8RIOF",
        title = "YOUR VALUABLE GIFT ENABLES US TO CONTINUE THE RACE TOWARD VICTORY",
        text = "Because of generous donors like you, we can advance one step closer to fixing Washington government. We will honor your kind contribution by conducting our campaign to reflect the determination and integrity that Daniel Webster is known for. Thank you for becoming a Webster Donor Warrior.",
        footer = "Paid for by Daniel Webster For Congress"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101011709/https://secure.joecollinsforcongress.com/donation1/?InitiativeKey=VE0D2VWDBDVQ",
        title = "Stand with Joe!",
        text = "As your next Congressman, I will start with the rebuilding of South Central LA! Moving FORWARD in working with businesses in our district to bring in quality paying jobs, working to help rebuild the infrastructure of our communities, to support growth and expansion of housing and businesses. Taking ACTION in helping to ensure our schools receive major funding to overhaul our education program. And helping the TRANSITION to help rebuild and foster the relationship between law enforcement and our community.",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201213063415/https://secure.squarespace.com/checkout/donate?donatePageId=5ecc811995a4d74aea67676d&ss_cid=edf464be-7b4b-4231-ac84-5ba180c28e78&ss_cvisit=1603868097604&ss_cvr=9cc107d3-7d3a-4075-a93e-aed4dc886250%7C1603868097515%7C1603868097515%7C1603868097515%7C1",
        ## Nothing
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005044822/https://secure.victorypassport.com/pages/handel/main-donate-page?location=https%3A%2F%2Fkarenhandel.com%2Fdonate&location_url=aHR0cHM6Ly9rYXJlbmhhbmRlbC5jb20vZG9uYXRlLyM%3D",
        title = "Karen is working to deliver real results to Georgia. Contribute today to join the team!",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113041602/https://spark.widgetmakr.com/widget/render/bc027835-dca5-4dc9-b691-cdb648713f67",
        # Widgetmakr retired; cannot see
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201031051317/https://www.trailblz.info/ColeTom/Donations.aspx?rdif=http%3A%2F%2Fwww.tomcoleforcongress.com/thank_you&ver=2",
        title = "Tom Cole for Congress",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101052905/http://vickyhartzler.com/donate",
        title = "Interested in Donating?",
        text = "Please contribute to Vicky Hartzler for Congress. Help us raise money and share our cause today!",
        footer = "2019 Paid for by Vicky Hartzler for Congress"
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112200422/https://billylongforcongress.com/donate",
        title = "Looking To Get Involved?",
        text = "Please contribute to Billy Long for Congress. Help us raise money and share our cause today!",
        footer = "2020 Paid for by Billy Long for Congress"
      )
    ) %>%
      bind_rows()
  )

# Test =========================================================================
assert_that(all(!is.na(senate$text)))
assert_that(all(!is.na(house$text)))
# assert_that(all(senate$text != ""))
# assert_that(all(house$text != ""))

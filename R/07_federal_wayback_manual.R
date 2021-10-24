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

# Duplicate scrapes per URL ====================================================
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
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201105050630/https://beeler4congress.nationbuilder.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201108222819/https://www.campaigncontribution.com/payment/contribution/info/29f7be14-f873-419c-bf56-7f5668bc41ff",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112201614/https://www.campaigncontribution.com/payment/contribution/info/57fe9544-1eb0-4626-bbf2-786b57def75f",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112181149/https://www.campaigncontribution.com/payment/contribution/info/6be33664-4500-45dd-a593-b2a47d017538",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201029095733/https://carlosforny.com/donation/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201109035506/https://www.chuckforcongress.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201030071543/https://www.cooperforcongress.com/form/-8877775747779720448",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101033434/http://www.erikastottspearson.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201024171436/https://www.garamendi.org/contribute",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005061620/https://garretgraves.ichooseapex.com/donate/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101082248/https://jim2020.com/donate/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201104042810/https://www.jimcosta.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005035729/https://johnbriscoe.us/donations",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201108075605/https://www.johnnyteague.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201114075103/https://mariodiazbalart.org/donate/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113162231/http://www.markreedforcongress.com/donate/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101003436/https://maryottforcongress.com/contribute",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201111181133/https://www.maureen4congress.com/donate/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201026124354/https://michaelguest.ms/donate/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201109064844/https://www.mikekellyforcongress.com/contribute/",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201106010248/https://www.mikethompsonforcongress.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005053153/https://mystique.victorypassport.com/pages/baird/baird-donations?location=https%3A%2F%2Fgive.victorypassport.com%2Fbaird%2Fdonate&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vYmFpcmQvZG9uYXRlLyM%3D",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005102621/https://mystique.victorypassport.com/pages/joyce/main-donation-page?location=https%3A%2F%2Fgive.victorypassport.com%2Fjoyce%2Fdonate-today&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vam95Y2UvZG9uYXRlLXRvZGF5LyM%3D",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113032152/https://www.parrott2020.org/donate-page",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113162754/https://petel4congress.com/donations/donation-form",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112204729/https://www.rabornforcongress.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201105084414/https://reygonzalez.org/donate-today.html",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201106011308/https://rishikumar.com/contribute",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201105043954/https://www.rogersforwisconsin.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201027122520/https://schakowsky.bsd.net/page/contribute/default?donate_page_KEY=15",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113162050/https://secure.campaignsolutions.com/calvert/list/proc/donation1/?InitiativeKey=LL39MZ8HTQXD",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101182721/https://secure.campaignsolutions.com/kenbuck/list/proc/donation1/?initiativekey=V0T9TNVLFGX5",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201115010830/https://secure.campaignsolutions.com/webster/donation1/default.aspx?InitiativeKey=8LI7KDO8RIOF",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101011709/https://secure.joecollinsforcongress.com/donation1/?InitiativeKey=VE0D2VWDBDVQ",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201114025921/https://secure.ngpvan.com/v1/Forms/-JGiuva5hk6bvLoBN7nHcw2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005072812/https://secure.ngpvan.com/v1/Forms/lZk2sknDFEaf2y9mFq1piQ2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201213043442/https://secure.ngpvan.com/v1/Forms/SmKLOj5DxkiO8Y96aSoDVQ2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201109202202/https://secure.ngpvan.com/v1/Forms/UzuWY0wU8EC3YlSdP5LoXg2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005101129/https://secure.ngpvan.com/v1/Forms/V5zS2tQN7UGegndBJW-oKw2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201105025206/https://secure.ngpvan.com/v1/Forms/VHSW6b1ewk-YWY8XloCLZw2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201212223847/https://secure.ngpvan.com/v1/Forms/WuiklsKYhk223Z9Pkldcyg2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201212231219/https://secure.ngpvan.com/v1/Forms/ZFgQB1NB_027aRtM8Xj6LQ2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113032149/https://secure.ngpvan.com/v1/Forms/zlW8zr2BKEa_YQBu8Pin8A2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201213063415/https://secure.squarespace.com/checkout/donate?donatePageId=5ecc811995a4d74aea67676d&ss_cid=edf464be-7b4b-4231-ac84-5ba180c28e78&ss_cvisit=1603868097604&ss_cvr=9cc107d3-7d3a-4075-a93e-aed4dc886250%7C1603868097515%7C1603868097515%7C1603868097515%7C1",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201005044822/https://secure.victorypassport.com/pages/handel/main-donate-page?location=https%3A%2F%2Fkarenhandel.com%2Fdonate&location_url=aHR0cHM6Ly9rYXJlbmhhbmRlbC5jb20vZG9uYXRlLyM%3D",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201113041602/https://spark.widgetmakr.com/widget/render/bc027835-dca5-4dc9-b691-cdb648713f67",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201031051317/https://www.trailblz.info/ColeTom/Donations.aspx?rdif=http%3A%2F%2Fwww.tomcoleforcongress.com/thank_you&ver=2",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201101052905/http://vickyhartzler.com/donate",
        title = "",
        text = "",
        footer = ""
      ),
      data.frame(
        link = "http://web.archive.org/web/20201112200422/https://billylongforcongress.com/donate",
        title = "",
        text = "",
        footer = ""
      )
    ) %>%
      bind_rows()
  )

# Test =========================================================================
assert_that(all(!is.na(senate$text)))
assert_that(all(!is.na(house$text)))
# assert_that(all(senate$text != ""))
# assert_that(all(house$text != ""))

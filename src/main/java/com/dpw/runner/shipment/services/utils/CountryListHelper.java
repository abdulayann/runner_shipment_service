package com.dpw.runner.shipment.services.utils;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Component
public class CountryListHelper {

    public static class ISO3166 {

        public static String getAlpha3FromAlpha2(String alpha2) {
            if(IsStringNullOrEmpty(alpha2))
                return null;
            ISO3166Country iso3166Country = fromAlpha2(alpha2);
            return iso3166Country.getAlpha3();
        }

        public static String getAlpha2FromAlpha3(String alpha3) {
            if(IsStringNullOrEmpty(alpha3))
                return null;
            ISO3166Country iso3166Country = fromAlpha3(alpha3);
            return iso3166Country.getAlpha2();
        }

        public static ISO3166Country fromAlpha3(String alpha3)
        {
            List<ISO3166Country> collection = buildCollection();
            var collect = collection.stream().filter(p -> Objects.equals(p.getAlpha3(), alpha3)).findFirst();
            return collect.orElse(new ISO3166Country());
        }

        public static ISO3166Country fromAlpha2(String alpha2)
        {
            List<ISO3166Country> collection = buildCollection();
            var collect = collection.stream().filter(p -> Objects.equals(p.getAlpha2(), alpha2)).findFirst();
            return collect.orElse(new ISO3166Country());
        }

        public static String getCountryNameByCode(String countryCode)
        {
            if (countryCode.length() == 2)
                return fromAlpha2(countryCode.toUpperCase()).getName();
            if (countryCode.length() == 3)
                return fromAlpha3(countryCode.toUpperCase()).getName();
            return StringUtility.getEmptyString();
        }

        private static List<ISO3166Country> buildCollection()
        {
            List<ISO3166Country> collection = new ArrayList<>();

            // This collection built from Wikipedia entry on ISO3166-1 on 9th Feb 2016

            collection.add(new ISO3166Country("Afghanistan", "AF", "AFG", 4));
            collection.add(new ISO3166Country("Åland Islands", "AX", "ALA", 248));
            collection.add(new ISO3166Country("Albania", "AL", "ALB", 8));
            collection.add(new ISO3166Country("Algeria", "DZ", "DZA", 12));
            collection.add(new ISO3166Country("American Samoa", "AS", "ASM", 16));
            collection.add(new ISO3166Country("Andorra", "AD", "AND", 20));
            collection.add(new ISO3166Country("Angola", "AO", "AGO", 24));
            collection.add(new ISO3166Country("Anguilla", "AI", "AIA", 660));
            collection.add(new ISO3166Country("Antarctica", "AQ", "ATA", 10));
            collection.add(new ISO3166Country("Antigua and Barbuda", "AG", "ATG", 28));
            collection.add(new ISO3166Country("Argentina", "AR", "ARG", 32));
            collection.add(new ISO3166Country("Armenia", "AM", "ARM", 51));
            collection.add(new ISO3166Country("Aruba", "AW", "ABW", 533));
            collection.add(new ISO3166Country("Australia", "AU", "AUS", 36));
            collection.add(new ISO3166Country("Austria", "AT", "AUT", 40));
            collection.add(new ISO3166Country("Azerbaijan", "AZ", "AZE", 31));
            collection.add(new ISO3166Country("Bahamas", "BS", "BHS", 44));
            collection.add(new ISO3166Country("Bahrain", "BH", "BHR", 48));
            collection.add(new ISO3166Country("Bangladesh", "BD", "BGD", 50));
            collection.add(new ISO3166Country("Barbados", "BB", "BRB", 52));
            collection.add(new ISO3166Country("Belarus", "BY", "BLR", 112));
            collection.add(new ISO3166Country("Belgium", "BE", "BEL", 56));
            collection.add(new ISO3166Country("Belize", "BZ", "BLZ", 84));
            collection.add(new ISO3166Country("Benin", "BJ", "BEN", 204));
            collection.add(new ISO3166Country("Bermuda", "BM", "BMU", 60));
            collection.add(new ISO3166Country("Bhutan", "BT", "BTN", 64));
            collection.add(new ISO3166Country("Bolivia (Plurinational State of)", "BO", "BOL", 68));
            collection.add(new ISO3166Country("Bonaire, Sint Eustatius and Saba", "BQ", "BES", 535));
            collection.add(new ISO3166Country("Bosnia and Herzegovina", "BA", "BIH", 70));
            collection.add(new ISO3166Country("Botswana", "BW", "BWA", 72));
            collection.add(new ISO3166Country("Bouvet Island", "BV", "BVT", 74));
            collection.add(new ISO3166Country("Brazil", "BR", "BRA", 76));
            collection.add(new ISO3166Country("British Indian Ocean Territory", "IO", "IOT", 86));
            collection.add(new ISO3166Country("Brunei Darussalam", "BN", "BRN", 96));
            collection.add(new ISO3166Country("Bulgaria", "BG", "BGR", 100));
            collection.add(new ISO3166Country("Burkina Faso", "BF", "BFA", 854));
            collection.add(new ISO3166Country("Burundi", "BI", "BDI", 108));
            collection.add(new ISO3166Country("Cabo Verde", "CV", "CPV", 132));
            collection.add(new ISO3166Country("Cambodia", "KH", "KHM", 116));
            collection.add(new ISO3166Country("Cameroon", "CM", "CMR", 120));
            collection.add(new ISO3166Country("Canada", "CA", "CAN", 124));
            collection.add(new ISO3166Country("Cayman Islands", "KY", "CYM", 136));
            collection.add(new ISO3166Country("Central African Republic", "CF", "CAF", 140));
            collection.add(new ISO3166Country("Chad", "TD", "TCD", 148));
            collection.add(new ISO3166Country("Chile", "CL", "CHL", 152));
            collection.add(new ISO3166Country("China", "CN", "CHN", 156));
            collection.add(new ISO3166Country("Christmas Island", "CX", "CXR", 162));
            collection.add(new ISO3166Country("Cocos (Keeling) Islands", "CC", "CCK", 166));
            collection.add(new ISO3166Country("Colombia", "CO", "COL", 170));
            collection.add(new ISO3166Country("Comoros", "KM", "COM", 174));
            collection.add(new ISO3166Country("Congo", "CG", "COG", 178));
            collection.add(new ISO3166Country("Congo (Democratic Republic of the)", "CD", "COD", 180));
            collection.add(new ISO3166Country("Cook Islands", "CK", "COK", 184));
            collection.add(new ISO3166Country("Costa Rica", "CR", "CRI", 188));
            collection.add(new ISO3166Country("Côte d'Ivoire", "CI", "CIV", 384));
            collection.add(new ISO3166Country("Croatia", "HR", "HRV", 191));
            collection.add(new ISO3166Country("Cuba", "CU", "CUB", 192));
            collection.add(new ISO3166Country("Curaçao", "CW", "CUW", 531));
            collection.add(new ISO3166Country("Cyprus", "CY", "CYP", 196));
            collection.add(new ISO3166Country("Czech Republic", "CZ", "CZE", 203));
            collection.add(new ISO3166Country("Denmark", "DK", "DNK", 208));
            collection.add(new ISO3166Country("Djibouti", "DJ", "DJI", 262));
            collection.add(new ISO3166Country("Dominica", "DM", "DMA", 212));
            collection.add(new ISO3166Country("Dominican Republic", "DO", "DOM", 214));
            collection.add(new ISO3166Country("Ecuador", "EC", "ECU", 218));
            collection.add(new ISO3166Country("Egypt", "EG", "EGY", 818));
            collection.add(new ISO3166Country("El Salvador", "SV", "SLV", 222));
            collection.add(new ISO3166Country("Equatorial Guinea", "GQ", "GNQ", 226));
            collection.add(new ISO3166Country("Eritrea", "ER", "ERI", 232));
            collection.add(new ISO3166Country("Estonia", "EE", "EST", 233));
            collection.add(new ISO3166Country("Ethiopia", "ET", "ETH", 231));
            collection.add(new ISO3166Country("Falkland Islands (Malvinas)", "FK", "FLK", 238));
            collection.add(new ISO3166Country("Faroe Islands", "FO", "FRO", 234));
            collection.add(new ISO3166Country("Fiji", "FJ", "FJI", 242));
            collection.add(new ISO3166Country("Finland", "FI", "FIN", 246));
            collection.add(new ISO3166Country("France", "FR", "FRA", 250));
            collection.add(new ISO3166Country("French Guiana", "GF", "GUF", 254));
            collection.add(new ISO3166Country("French Polynesia", "PF", "PYF", 258));
            collection.add(new ISO3166Country("French Southern Territories", "TF", "ATF", 260));
            collection.add(new ISO3166Country("Gabon", "GA", "GAB", 266));
            collection.add(new ISO3166Country("Gambia", "GM", "GMB", 270));
            collection.add(new ISO3166Country("Georgia", "GE", "GEO", 268));
            collection.add(new ISO3166Country("Germany", "DE", "DEU", 276));
            collection.add(new ISO3166Country("Ghana", "GH", "GHA", 288));
            collection.add(new ISO3166Country("Gibraltar", "GI", "GIB", 292));
            collection.add(new ISO3166Country("Greece", "GR", "GRC", 300));
            collection.add(new ISO3166Country("Greenland", "GL", "GRL", 304));
            collection.add(new ISO3166Country("Grenada", "GD", "GRD", 308));
            collection.add(new ISO3166Country("Guadeloupe", "GP", "GLP", 312));
            collection.add(new ISO3166Country("Guam", "GU", "GUM", 316));
            collection.add(new ISO3166Country("Guatemala", "GT", "GTM", 320));
            collection.add(new ISO3166Country("Guernsey", "GG", "GGY", 831));
            collection.add(new ISO3166Country("Guinea", "GN", "GIN", 324));
            collection.add(new ISO3166Country("Guinea-Bissau", "GW", "GNB", 624));
            collection.add(new ISO3166Country("Guyana", "GY", "GUY", 328));
            collection.add(new ISO3166Country("Haiti", "HT", "HTI", 332));
            collection.add(new ISO3166Country("Heard Island and McDonald Islands", "HM", "HMD", 334));
            collection.add(new ISO3166Country("Holy See", "VA", "VAT", 336));
            collection.add(new ISO3166Country("Honduras", "HN", "HND", 340));
            collection.add(new ISO3166Country("Hong Kong", "HK", "HKG", 344));
            collection.add(new ISO3166Country("Hungary", "HU", "HUN", 348));
            collection.add(new ISO3166Country("Iceland", "IS", "ISL", 352));
            collection.add(new ISO3166Country("India", "IN", "IND", 356));
            collection.add(new ISO3166Country("Indonesia", "ID", "IDN", 360));
            collection.add(new ISO3166Country("Iran (Islamic Republic of)", "IR", "IRN", 364));
            collection.add(new ISO3166Country("Iraq", "IQ", "IRQ", 368));
            collection.add(new ISO3166Country("Ireland", "IE", "IRL", 372));
            collection.add(new ISO3166Country("Isle of Man", "IM", "IMN", 833));
            collection.add(new ISO3166Country("Israel", "IL", "ISR", 376));
            collection.add(new ISO3166Country("Italy", "IT", "ITA", 380));
            collection.add(new ISO3166Country("Jamaica", "JM", "JAM", 388));
            collection.add(new ISO3166Country("Japan", "JP", "JPN", 392));
            collection.add(new ISO3166Country("Jersey", "JE", "JEY", 832));
            collection.add(new ISO3166Country("Jordan", "JO", "JOR", 400));
            collection.add(new ISO3166Country("Kazakhstan", "KZ", "KAZ", 398));
            collection.add(new ISO3166Country("Kenya", "KE", "KEN", 404));
            collection.add(new ISO3166Country("Kiribati", "KI", "KIR", 296));
            collection.add(new ISO3166Country("Korea (Democratic People's Republic of)", "KP", "PRK", 408));
            collection.add(new ISO3166Country("Korea (Republic of)", "KR", "KOR", 410));
            collection.add(new ISO3166Country("Kuwait", "KW", "KWT", 414));
            collection.add(new ISO3166Country("Kyrgyzstan", "KG", "KGZ", 417));
            collection.add(new ISO3166Country("Lao People's Democratic Republic", "LA", "LAO", 418));
            collection.add(new ISO3166Country("Latvia", "LV", "LVA", 428));
            collection.add(new ISO3166Country("Lebanon", "LB", "LBN", 422));
            collection.add(new ISO3166Country("Lesotho", "LS", "LSO", 426));
            collection.add(new ISO3166Country("Liberia", "LR", "LBR", 430));
            collection.add(new ISO3166Country("Libya", "LY", "LBY", 434));
            collection.add(new ISO3166Country("Liechtenstein", "LI", "LIE", 438));
            collection.add(new ISO3166Country("Lithuania", "LT", "LTU", 440));
            collection.add(new ISO3166Country("Luxembourg", "LU", "LUX", 442));
            collection.add(new ISO3166Country("Macao", "MO", "MAC", 446));
            collection.add(new ISO3166Country("Macedonia (the former Yugoslav Republic of)", "MK", "MKD", 807));
            collection.add(new ISO3166Country("Madagascar", "MG", "MDG", 450));
            collection.add(new ISO3166Country("Malawi", "MW", "MWI", 454));
            collection.add(new ISO3166Country("Malaysia", "MY", "MYS", 458));
            collection.add(new ISO3166Country("Maldives", "MV", "MDV", 462));
            collection.add(new ISO3166Country("Mali", "ML", "MLI", 466));
            collection.add(new ISO3166Country("Malta", "MT", "MLT", 470));
            collection.add(new ISO3166Country("Marshall Islands", "MH", "MHL", 584));
            collection.add(new ISO3166Country("Martinique", "MQ", "MTQ", 474));
            collection.add(new ISO3166Country("Mauritania", "MR", "MRT", 478));
            collection.add(new ISO3166Country("Mauritius", "MU", "MUS", 480));
            collection.add(new ISO3166Country("Mayotte", "YT", "MYT", 175));
            collection.add(new ISO3166Country("Mexico", "MX", "MEX", 484));
            collection.add(new ISO3166Country("Micronesia (Federated States of)", "FM", "FSM", 583));
            collection.add(new ISO3166Country("Moldova (Republic of)", "MD", "MDA", 498));
            collection.add(new ISO3166Country("Monaco", "MC", "MCO", 492));
            collection.add(new ISO3166Country("Mongolia", "MN", "MNG", 496));
            collection.add(new ISO3166Country("Montenegro", "ME", "MNE", 499));
            collection.add(new ISO3166Country("Montserrat", "MS", "MSR", 500));
            collection.add(new ISO3166Country("Morocco", "MA", "MAR", 504));
            collection.add(new ISO3166Country("Mozambique", "MZ", "MOZ", 508));
            collection.add(new ISO3166Country("Myanmar", "MM", "MMR", 104));
            collection.add(new ISO3166Country("Namibia", "NA", "NAM", 516));
            collection.add(new ISO3166Country("Nauru", "NR", "NRU", 520));
            collection.add(new ISO3166Country("Nepal", "NP", "NPL", 524));
            collection.add(new ISO3166Country("Netherlands", "NL", "NLD", 528));
            collection.add(new ISO3166Country("New Caledonia", "NC", "NCL", 540));
            collection.add(new ISO3166Country("New Zealand", "NZ", "NZL", 554));
            collection.add(new ISO3166Country("Nicaragua", "NI", "NIC", 558));
            collection.add(new ISO3166Country("Niger", "NE", "NER", 562));
            collection.add(new ISO3166Country("Nigeria", "NG", "NGA", 566));
            collection.add(new ISO3166Country("Niue", "NU", "NIU", 570));
            collection.add(new ISO3166Country("Norfolk Island", "NF", "NFK", 574));
            collection.add(new ISO3166Country("Northern Mariana Islands", "MP", "MNP", 580));
            collection.add(new ISO3166Country("Norway", "NO", "NOR", 578));
            collection.add(new ISO3166Country("Oman", "OM", "OMN", 512));
            collection.add(new ISO3166Country("Pakistan", "PK", "PAK", 586));
            collection.add(new ISO3166Country("Palau", "PW", "PLW", 585));
            collection.add(new ISO3166Country("Palestine, State of", "PS", "PSE", 275));
            collection.add(new ISO3166Country("Panama", "PA", "PAN", 591));
            collection.add(new ISO3166Country("Papua New Guinea", "PG", "PNG", 598));
            collection.add(new ISO3166Country("Paraguay", "PY", "PRY", 600));
            collection.add(new ISO3166Country("Peru", "PE", "PER", 604));
            collection.add(new ISO3166Country("Philippines", "PH", "PHL", 608));
            collection.add(new ISO3166Country("Pitcairn", "PN", "PCN", 612));
            collection.add(new ISO3166Country("Poland", "PL", "POL", 616));
            collection.add(new ISO3166Country("Portugal", "PT", "PRT", 620));
            collection.add(new ISO3166Country("Puerto Rico", "PR", "PRI", 630));
            collection.add(new ISO3166Country("Qatar", "QA", "QAT", 634));
            collection.add(new ISO3166Country("Réunion", "RE", "REU", 638));
            collection.add(new ISO3166Country("Romania", "RO", "ROU", 642));
            collection.add(new ISO3166Country("Russian Federation", "RU", "RUS", 643));
            collection.add(new ISO3166Country("Rwanda", "RW", "RWA", 646));
            collection.add(new ISO3166Country("Saint Barthélemy", "BL", "BLM", 652));
            collection.add(new ISO3166Country("Saint Helena, Ascension and Tristan da Cunha", "SH", "SHN", 654));
            collection.add(new ISO3166Country("Saint Kitts and Nevis", "KN", "KNA", 659));
            collection.add(new ISO3166Country("Saint Lucia", "LC", "LCA", 662));
            collection.add(new ISO3166Country("Saint Martin (French part)", "MF", "MAF", 663));
            collection.add(new ISO3166Country("Saint Pierre and Miquelon", "PM", "SPM", 666));
            collection.add(new ISO3166Country("Saint Vincent and the Grenadines", "VC", "VCT", 670));
            collection.add(new ISO3166Country("Samoa", "WS", "WSM", 882));
            collection.add(new ISO3166Country("San Marino", "SM", "SMR", 674));
            collection.add(new ISO3166Country("Sao Tome and Principe", "ST", "STP", 678));
            collection.add(new ISO3166Country("Saudi Arabia", "SA", "SAU", 682));
            collection.add(new ISO3166Country("Senegal", "SN", "SEN", 686));
            collection.add(new ISO3166Country("Serbia", "RS", "SRB", 688));
            collection.add(new ISO3166Country("Seychelles", "SC", "SYC", 690));
            collection.add(new ISO3166Country("Sierra Leone", "SL", "SLE", 694));
            collection.add(new ISO3166Country("Singapore", "SG", "SGP", 702));
            collection.add(new ISO3166Country("Sint Maarten (Dutch part)", "SX", "SXM", 534));
            collection.add(new ISO3166Country("Slovakia", "SK", "SVK", 703));
            collection.add(new ISO3166Country("Slovenia", "SI", "SVN", 705));
            collection.add(new ISO3166Country("Solomon Islands", "SB", "SLB", 90));
            collection.add(new ISO3166Country("Somalia", "SO", "SOM", 706));
            collection.add(new ISO3166Country("South Africa", "ZA", "ZAF", 710));
            collection.add(new ISO3166Country("South Georgia and the South Sandwich Islands", "GS", "SGS", 239));
            collection.add(new ISO3166Country("South Sudan", "SS", "SSD", 728));
            collection.add(new ISO3166Country("Spain", "ES", "ESP", 724));
            collection.add(new ISO3166Country("Sri Lanka", "LK", "LKA", 144));
            collection.add(new ISO3166Country("Sudan", "SD", "SDN", 729));
            collection.add(new ISO3166Country("Suriname", "SR", "SUR", 740));
            collection.add(new ISO3166Country("Svalbard and Jan Mayen", "SJ", "SJM", 744));
            collection.add(new ISO3166Country("Swaziland", "SZ", "SWZ", 748));
            collection.add(new ISO3166Country("Sweden", "SE", "SWE", 752));
            collection.add(new ISO3166Country("Switzerland", "CH", "CHE", 756));
            collection.add(new ISO3166Country("Syrian Arab Republic", "SY", "SYR", 760));
            collection.add(new ISO3166Country("Taiwan, Province of China[a]", "TW", "TWN", 158));
            collection.add(new ISO3166Country("Tajikistan", "TJ", "TJK", 762));
            collection.add(new ISO3166Country("Tanzania, United Republic of", "TZ", "TZA", 834));
            collection.add(new ISO3166Country("Thailand", "TH", "THA", 764));
            collection.add(new ISO3166Country("Timor-Leste", "TL", "TLS", 626));
            collection.add(new ISO3166Country("Togo", "TG", "TGO", 768));
            collection.add(new ISO3166Country("Tokelau", "TK", "TKL", 772));
            collection.add(new ISO3166Country("Tonga", "TO", "TON", 776));
            collection.add(new ISO3166Country("Trinidad and Tobago", "TT", "TTO", 780));
            collection.add(new ISO3166Country("Tunisia", "TN", "TUN", 788));
            collection.add(new ISO3166Country("Turkey", "TR", "TUR", 792));
            collection.add(new ISO3166Country("Turkmenistan", "TM", "TKM", 795));
            collection.add(new ISO3166Country("Turks and Caicos Islands", "TC", "TCA", 796));
            collection.add(new ISO3166Country("Tuvalu", "TV", "TUV", 798));
            collection.add(new ISO3166Country("Uganda", "UG", "UGA", 800));
            collection.add(new ISO3166Country("Ukraine", "UA", "UKR", 804));
            collection.add(new ISO3166Country("United Arab Emirates", "AE", "ARE", 784));
            collection.add(new ISO3166Country("United Kingdom of Great Britain and Northern Ireland", "GB", "GBR", 826));
            collection.add(new ISO3166Country("United States of America", "US", "USA", 840));
            collection.add(new ISO3166Country("United States Minor Outlying Islands", "UM", "UMI", 581));
            collection.add(new ISO3166Country("Uruguay", "UY", "URY", 858));
            collection.add(new ISO3166Country("Uzbekistan", "UZ", "UZB", 860));
            collection.add(new ISO3166Country("Vanuatu", "VU", "VUT", 548));
            collection.add(new ISO3166Country("Venezuela (Bolivarian Republic of)", "VE", "VEN", 862));
            collection.add(new ISO3166Country("Viet Nam", "VN", "VNM", 704));
            collection.add(new ISO3166Country("Virgin Islands (British)", "VG", "VGB", 92));
            collection.add(new ISO3166Country("Virgin Islands (U.S.)", "VI", "VIR", 850));
            collection.add(new ISO3166Country("Wallis and Futuna", "WF", "WLF", 876));
            collection.add(new ISO3166Country("Western Sahara", "EH", "ESH", 732));
            collection.add(new ISO3166Country("Yemen", "YE", "YEM", 887));
            collection.add(new ISO3166Country("Zambia", "ZM", "ZMB", 894));
            collection.add(new ISO3166Country("Zimbabwe", "ZW", "ZWE", 716));

            return collection;
        }
    }



    @Data
    @NoArgsConstructor
    public static class ISO3166Country {
        public ISO3166Country(String name, String alpha2, String alpha3, int numericCode)
        {
            this.name = name;
            this.alpha2 = alpha2;
            this.alpha3 = alpha3;
            this.numericCode = numericCode;
        }

        private String name;

        private String alpha2;

        private String alpha3;

        private int numericCode;
    }

}

package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DbAccessHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.TextStyle;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Component
@SuppressWarnings("java:S1172")
public class GetNextNumberHelper {

    @Autowired
    private IProductSequenceConfigDao productSequenceConfigDao;

    @Autowired
    private IShipmentSettingsSync shipmentSettingsSync;
    @Autowired
    private V1AuthHelper v1AuthHelper;

    public String generateCustomSequence(ProductSequenceConfig sequenceSettings, String regexPattern, int tenantId,
                                         boolean updateCounter, UsersDto user, boolean updateBranchCode) throws RunnerException {
        if (regexPattern.isEmpty()) {
            throw new RunnerException("RegexExpression can't be empty or null");
        }
        int startPosition = regexPattern.indexOf("{");
        String prefix =
            startPosition == -1 ? regexPattern : regexPattern.substring(0, startPosition); // prefix
        String suffix = "";
        if (sequenceSettings.getGenerationType() == GenerationType.Regex) {
            Pattern p = Pattern.compile("\\{([^}]*)\\}"); // original v1 regex @"(?<={)[\w;]{1,}(?=})"
            Matcher matches = p.matcher(regexPattern);
            var valueOf = new HashMap<String, String>();
            LocalDateTime currDate = LocalDateTime.now();

            valueOf.put(Constants.BRANCH, "BR"); // branch is not clear
            valueOf.put("dd", DateTimeFormatter.ofPattern("dd").format(currDate));
            valueOf.put("yy", Integer.toString(currDate.getYear()).substring(2)); // last 2 digits
            valueOf.put("mm", padLeft(Integer.toString(currDate.getMonthValue()), 2, '0'));
            valueOf.put("yyyy", Integer.toString(currDate.getYear()));
            valueOf.put("mon", currDate.getMonth().getDisplayName(TextStyle.SHORT, Locale.ROOT));
            DateTimeFormatter df = DateTimeFormatter.ofPattern("MMMM");
            // Format the date to get the month in "MMMM" format
            String monthName = df.format(currDate);
            valueOf.put("month", monthName);
            valueOf.put("cc", ""); // Empty string
            valueOf.put("seq", ""); // Empty string

            List<String> segments = new ArrayList<>();
            while (matches.find()) {
                segments.add(matches.group(1) != null ? matches.group(1) : matches.group(2));
            }
            // Filter out empty or whitespace-only segments
            segments = segments.stream().filter(s -> !s.trim().isEmpty()).toList();

            String resetFreq = determineResetFrequency(segments);

            for (var word : segments) {
                List<String> wordSplit = List.of(word.split(";"));
                if (valueOf.get(wordSplit.get(0).toLowerCase()) == null) {
                    throw new ValidationException("CONFIGURED_SEQUENCE_REGEX_VALIDATION");
                }
                suffix = getSuffixValue(sequenceSettings, user, updateBranchCode, wordSplit, suffix, valueOf, resetFreq);
            }
        }
        else if (sequenceSettings.getGenerationType() == GenerationType.Random) {
            suffix = StringUtility.getRandomString(10);
        }
        else if (sequenceSettings.getGenerationType() == GenerationType.Serial) {
            suffix = sequenceSettings.getSerialCounter().toString();
            sequenceSettings.setSerialCounter(sequenceSettings.getSerialCounter() + 1);
        }
        if (prefix.length() + suffix.length() > 50) {
            throw new ValidationException("CONFIGURED_SEQUENCE_LENGTH_VALIDATION");
        }
        if (updateCounter) {
            log.info("CR-ID {} || Calling event {} from generateCustomSequence", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PRODUCT_SEQ_SAVE);
            sequenceSettings = productSequenceConfigDao.save(sequenceSettings);
            try {
                shipmentSettingsSync.syncProductSequence(sequenceSettings, v1AuthHelper.getHeadersForDataSync());
            } catch (Exception e) {
                log.error("Error performing sync on shipment settings product sequence entity", e);
            }
        }
        return prefix + suffix;
    }

    public static String determineResetFrequency(List<String> segments) {
        boolean isDailyReset = false;
        boolean isMonthlyReset = false;
        boolean isYearlyReset = false;

        for (String word : segments)
        {
            var wordSplit = word.split(";");
            String key = wordSplit[0].toLowerCase();

            // Handle individual date tokens
            if ("dd".equalsIgnoreCase(key))
                isDailyReset = true;
            else if ("mm".equalsIgnoreCase(key) || "mon".equalsIgnoreCase(key) || "month".equalsIgnoreCase(key))
                isMonthlyReset = true;
            else if ("yy".equalsIgnoreCase(key) || "yyyy".equalsIgnoreCase(key))
                isYearlyReset = true;
                // Handle date format strings
            else if ("date".equalsIgnoreCase(key) && wordSplit.length > 1)
            {
                String dateFormat = wordSplit[1].toLowerCase();

                // Check if format contains day components
                if (dateFormat.contains("dd"))
                    isDailyReset = true;
                // Check if format contains month components
                if (dateFormat.contains("mm") || dateFormat.contains("mon") || dateFormat.contains("month"))
                    isMonthlyReset = true;
                // Check if format contains year components
                if (dateFormat.contains("yy") || dateFormat.contains("yyyy"))
                    isYearlyReset = true;
            }
        }

        // Priority: daily > monthly > yearly
        if (isDailyReset)
            return "daily";
        if (isMonthlyReset)
            return "monthly";
        if (isYearlyReset)
            return "yearly";

        return "";
    }

    @NotNull
    private String getSuffixValue(ProductSequenceConfig sequenceSettings, UsersDto user, boolean updateBranchCode, List<String> wordSplit, String suffix, HashMap<String, String> valueOf, String resetFreq) throws RunnerException {
        if (wordSplit.size() > 1) {
            if (wordSplit.get(0).equalsIgnoreCase("seq")) {
                if(Strings.isNullOrEmpty(resetFreq))
                    resetFreq = wordSplit.size() > 2 ? wordSplit.get(2) : "Never";
                suffix += padLeft(
                    getNextRegexSequenceNumber(sequenceSettings, resetFreq),
                    Integer.parseInt(wordSplit.get(1)),
                    '0');
            }
            else {
                suffix += padLeft(
                    valueOf.get(wordSplit.get(0).toLowerCase()),
                    Integer.parseInt(wordSplit.get(1)),
                    '0');
            }
        }
        else if (updateBranchCode && wordSplit.get(0).equalsIgnoreCase(Constants.BRANCH)) {
            if (user != null) {
                valueOf.put(Constants.BRANCH, user.getCode());
            }
            suffix += valueOf.get(wordSplit.get(0).toLowerCase());
        }
        else suffix += valueOf.get(wordSplit.get(0).toLowerCase());
        return suffix;
    }

    public String getNextRegexSequenceNumber(ProductSequenceConfig sequenceSettings, String resetFreq) throws RunnerException {
        boolean resetCounter = needResetCounter(sequenceSettings, resetFreq);
        if (resetCounter) {
            sequenceSettings.setSerialCounter(0);
            sequenceSettings.setSequenceStartTime(LocalDateTime.now(Clock.systemUTC()));
        }
        sequenceSettings.setSerialCounter(sequenceSettings.getSerialCounter() + 1);
        return sequenceSettings.getSerialCounter().toString();
    }

    public static boolean needResetCounter(ProductSequenceConfig sequenceSettings, String resetFreq) throws RunnerException {

        LocalDateTime localTimeStart;
        LocalDateTime localTimeNow;

        String timeZoneId = UserContext.getUser().TimeZoneId;
        if (timeZoneId == null || timeZoneId.isEmpty())
            throw new RunnerException("TimeZoneId Required if resetFreq is Daily");

        TimeZone localZone = TimeZone.getTimeZone(timeZoneId);
        localTimeNow =
                (LocalDateTime.now(Clock.systemUTC())).atZone(localZone.toZoneId()).toLocalDateTime();
        localTimeStart =
                sequenceSettings.getSequenceStartTime() != null
                        ? sequenceSettings
                        .getSequenceStartTime()
                        .atZone(localZone.toZoneId())
                        .toLocalDateTime()
                        : localTimeNow;
        boolean resetCounter = sequenceSettings
                .getSequenceStartTime() == null;


        if ("daily".equalsIgnoreCase(resetFreq) || "monthly".equalsIgnoreCase(resetFreq) || "yearly".equalsIgnoreCase(resetFreq))
        {
            if (localTimeStart.toLocalDate().isBefore(localTimeNow.toLocalDate())  && "daily".equalsIgnoreCase(resetFreq))
                resetCounter = true;
            else if ((localTimeStart.toLocalDate().getMonthValue() < localTimeNow.toLocalDate().getMonthValue() || localTimeStart.toLocalDate().getYear() < localTimeNow.toLocalDate().getYear()) && "monthly".equalsIgnoreCase(resetFreq))
                resetCounter = true;
            else if (localTimeStart.toLocalDate().getYear() < localTimeNow.toLocalDate().getYear() && "yearly".equalsIgnoreCase(resetFreq))
                resetCounter = true;
        }
        return resetCounter;
    }

    public ProductSequenceConfig getProductSequence(Long productId, ProductProcessTypes processType) {
        FilterCriteria entityIdCriteria =
            FilterCriteria.builder()
                .innerFilter(
                    Arrays.asList(
                        FilterCriteria.builder()
                            .criteria(
                                Criteria.builder()
                                    .fieldName("tenantProductId")
                                    .operator("=")
                                    .value(productId)
                                    .build())
                            .build(),
                        FilterCriteria.builder()
                            .logicOperator("AND")
                            .criteria(
                                Criteria.builder()
                                    .fieldName(Constants.PRODUCT_PROCESS_TYPES)
                                    .operator("=")
                                    .value(processType.toString())
                                    .build())
                            .build()))
                .build();

        ListCommonRequest listCommonRequest =
            ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(Integer.MAX_VALUE)
                .filterCriteria(Collections.singletonList(entityIdCriteria))
                .build();

        Map<String, RunnerEntityMapping> tableNames =
            Map.ofEntries(
                Map.entry(
                    "tenantProductId",
                    RunnerEntityMapping.builder()
                        .tableName("tenantProducts")
                        .dataType(Long.class)
                        .fieldName("id")
                        .build()),
                Map.entry(
                    Constants.PRODUCT_PROCESS_TYPES,
                    RunnerEntityMapping.builder()
                        .tableName("ProductSequenceConfig")
                        .dataType(ProductProcessTypes.class)
                        .fieldName(Constants.PRODUCT_PROCESS_TYPES)
                        .build()));

        Pair<Specification<ProductSequenceConfig>, Pageable> pair = DbAccessHelper.fetchData(listCommonRequest, ProductSequenceConfig.class, tableNames);
        return productSequenceConfigDao.findAndLock(pair.getLeft(), pair.getRight());
    }

    public String padLeft(String input, int len, char c) {
        if (input.length() >= len) {
            return input;
        }
        StringBuilder sb = new StringBuilder();
        while (sb.length() < len - input.length()) {
            sb.append(c);
        }
        sb.append(input);

        return sb.toString();
    }
}

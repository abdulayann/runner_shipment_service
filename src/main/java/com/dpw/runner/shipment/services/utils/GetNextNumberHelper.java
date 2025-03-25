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
        //        CompaniesRow companiesRow = null;
        if (sequenceSettings.getGenerationType() == GenerationType.Regex) {
            Pattern p = Pattern.compile("\\{([^}]*)\\}"); // original v1 regex @"(?<={)[\w;]{1,}(?=})"
            Matcher matches = p.matcher(regexPattern);
            var ValueOf = new HashMap<String, String>();
            LocalDateTime currDate = LocalDateTime.now();

            ValueOf.put(Constants.BRANCH, "BR"); // branch is not clear
            ValueOf.put("dd", DateTimeFormatter.ofPattern("dd").format(currDate));
            ValueOf.put("yy", Integer.valueOf(currDate.getYear()).toString().substring(2)); // last 2 digits
            ValueOf.put("mm", padLeft(Integer.valueOf(currDate.getMonthValue()).toString(), 2, '0'));
            ValueOf.put("yyyy", Integer.valueOf(currDate.getYear()).toString());
            ValueOf.put("mon", currDate.getMonth().getDisplayName(TextStyle.SHORT, Locale.ROOT));
            DateTimeFormatter df = DateTimeFormatter.ofPattern("MMMM");
            // Format the date to get the month in "MMMM" format
            String monthName = df.format(currDate);
            ValueOf.put("month", monthName);
            ValueOf.put("cc", ""); // Empty string
            ValueOf.put("seq", ""); // Empty string

            while (matches.find()) {
                String word = matches.group(1);
                List<String> wordSplit = List.of(word.split(";"));
                if (ValueOf.get(wordSplit.get(0).toLowerCase()) == null) {
                    throw new ValidationException("CONFIGURED_SEQUENCE_REGEX_VALIDATION");
                }
                suffix = getSuffixValue(sequenceSettings, user, updateBranchCode, wordSplit, suffix, ValueOf);
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

    @NotNull
    private String getSuffixValue(ProductSequenceConfig sequenceSettings, UsersDto user, boolean updateBranchCode, List<String> wordSplit, String suffix, HashMap<String, String> ValueOf) throws RunnerException {
        if (wordSplit.size() > 1) {
            if (wordSplit.get(0).equalsIgnoreCase("seq")) {
                String resetFreq = wordSplit.size() > 2 ? wordSplit.get(2) : "Never";
                suffix += padLeft(
                    GetNextRegexSequenceNumber(sequenceSettings, resetFreq),
                    Integer.parseInt(wordSplit.get(1)),
                    '0');
            }
            else {
                suffix += padLeft(
                    ValueOf.get(wordSplit.get(0).toLowerCase()),
                    Integer.parseInt(wordSplit.get(1)),
                    '0');
            }
        }
        else if (updateBranchCode && wordSplit.get(0).equalsIgnoreCase(Constants.BRANCH)) {
            if (user != null) {
                ValueOf.put(Constants.BRANCH, user.getCode());
            }
            suffix += ValueOf.get(wordSplit.get(0).toLowerCase());
        }
        else suffix += ValueOf.get(wordSplit.get(0).toLowerCase());
        return suffix;
    }

    public String GetNextRegexSequenceNumber(ProductSequenceConfig sequenceSettings, String resetFreq) throws RunnerException {
        LocalDateTime seqStartTime = sequenceSettings.getSequenceStartTime();
        boolean resetCounter = seqStartTime == null;
        if (resetFreq.equalsIgnoreCase("daily")) {
            LocalDateTime localTimeStart, localTimeNow;

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

            if (localTimeStart.isBefore(localTimeNow)) resetCounter = true;
        }
        if (resetCounter) {
            sequenceSettings.setSerialCounter(0);
            sequenceSettings.setSequenceStartTime(LocalDateTime.now(Clock.systemUTC()));
        }
        sequenceSettings.setSerialCounter(sequenceSettings.getSerialCounter() + 1);
        return sequenceSettings.getSerialCounter().toString();
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

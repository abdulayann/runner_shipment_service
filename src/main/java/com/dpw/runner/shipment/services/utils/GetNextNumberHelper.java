package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import java.text.SimpleDateFormat;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.TextStyle;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class GetNextNumberHelper {

  @Autowired private static IProductSequenceConfigDao productSequenceConfigDao;

  public static String generateCustomSequence(
      ProductSequenceConfig sequenceSettings,
      String regexPattern,
      int tenantId,
      boolean updateCounter,
      UsersDto user,
      boolean updateBranchCode) {
    if (regexPattern.isEmpty()) {
      throw new RunnerException("RegexExpression can't be empty or null");
    }
    // TODO revisit
    int startPosition = regexPattern.indexOf("{");
    String prefix =
        startPosition == -1 ? regexPattern : regexPattern.substring(0, startPosition); // prefix
    String suffix = "";
    //        CompaniesRow companiesRow = null;
    if (sequenceSettings.getGenerationType() == GenerationType.Regex) {
      Pattern p =
          Pattern.compile("(?<=\\{)[\\w;]{1,}(?=\\})"); // original v1 regex @"(?<={)[\w;]{1,}(?=})"
      Matcher matches = p.matcher(regexPattern);
      var ValueOf = new HashMap<String, String>();
      LocalDateTime currDate = LocalDateTime.now();

      ValueOf.put("branch", "BR"); // branch is not clear
      ValueOf.put("dd", Integer.valueOf(currDate.getDayOfMonth()).toString());
      ValueOf.put(
          "yy", Integer.valueOf(currDate.getYear()).toString().substring(2)); // last 2 digits
      ValueOf.put("mm", padLeft(Integer.valueOf(currDate.getMonthValue()).toString(), 2, '0'));
      ValueOf.put("yyyy", Integer.valueOf(currDate.getYear()).toString());
      ValueOf.put("mon", currDate.getMonth().getDisplayName(TextStyle.SHORT, Locale.ROOT));
      SimpleDateFormat sdf = new SimpleDateFormat("MMMM");
      // Format the date to get the month in "MMMM" format
      String monthName = sdf.format(currDate);
      ValueOf.put("month", monthName);
      ValueOf.put("cc", ""); // Empty string
      ValueOf.put("seq", ""); // Empty string

      while (matches.find()) {
        String word = matches.group();
        List<String> wordSplit = List.of(word.split(";"));
        if (ValueOf.get(wordSplit.get(0).toLowerCase()) == null) {
          throw new ValidationException("CONFIGURED_SEQUENCE_REGEX_VALIDATION");
        }
        if (wordSplit.size() > 1) {
          if (wordSplit.get(0).equalsIgnoreCase("seq")) {
            String resetFreq = wordSplit.size() > 2 ? wordSplit.get(2) : "Never";
            suffix +=
                padLeft(
                    GetNextRegexSequenceNumber(sequenceSettings, tenantId, resetFreq),
                    Integer.parseInt(wordSplit.get(1)),
                    '0');
          } else
            suffix +=
                padLeft(
                    ValueOf.get(wordSplit.get(0).toLowerCase()),
                    Integer.parseInt(wordSplit.get(1)),
                    '0');
        }
        // Ignoring this case for now TODO post clarification
        //                else if (wordSplit.get(0).equalsIgnoreCase("cc")) {
        //                    if (companiesRow == null && user != null && user.CompanyId != null)
        //                    {
        //                        companiesRow = uow.Connection.List<CompaniesRow>(new
        // Criteria("Id").In(user.CompanyId)).FirstOrDefault();
        //                        ValueOf["cc"] = companiesRow.Code;
        //                    }
        //                    suffix += ValueOf.get(wordSplit.get(0).toLowerCase());
        //                }
        else if (updateBranchCode && wordSplit.get(0).equalsIgnoreCase("branch")) {
          if (user != null) {
            ValueOf.put("branch", user.TenantCode);
          }
          suffix += ValueOf.get(wordSplit.get(0).toLowerCase());
        } else suffix += ValueOf.get(wordSplit.get(0).toLowerCase());
      }
    } else if (sequenceSettings.getGenerationType() == GenerationType.Random) {
      suffix = StringUtility.getRandomString(10);
    } else if (sequenceSettings.getGenerationType() == GenerationType.Serial) {
      suffix = sequenceSettings.getSerialCounter().toString();
      sequenceSettings.setSerialCounter(sequenceSettings.getSerialCounter() + 1);
    }
    if (prefix.length() + suffix.length() > 50) {
      throw new ValidationException("CONFIGURED_SEQUENCE_LENGTH_VALIDATION");
    }
    if (updateCounter) {
      productSequenceConfigDao.save(sequenceSettings);
    }
    return prefix + suffix;
  }

  public static String GetNextRegexSequenceNumber(
      ProductSequenceConfig sequenceSettings, int TenantId, String resetFreq) {
    LocalDateTime seqStartTime = sequenceSettings.getSequenceStartTime();
    boolean resetCounter = seqStartTime == null;
    if (resetFreq.equalsIgnoreCase("daily")) {
      LocalDateTime localTimeStart, localTimeNow;

      /*var retreq = new RetrieveRequest();
      retreq.EntityId = TenantId;
      TenantsRow tenant = new TenantsRepository().GetTenantByTenantId(uow.Connection, TenantId);
      String TimeZoneId = tenant.TimeZoneId;*/

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

  // TODO criteria to find the Product sequence
  public static ProductSequenceConfig getProductSequence(
      Long productId, ProductProcessTypes processType) {
    FilterCriteria entityIdCriteria =
        FilterCriteria.builder()
            .innerFilter(
                Arrays.asList(
                    FilterCriteria.builder()
                        .criteria(
                            Criteria.builder()
                                .fieldName("tenantProducts.id")
                                .operator("=")
                                .value(productId)
                                .build())
                        .build(),
                    FilterCriteria.builder()
                        .logicOperator("AND")
                        .criteria(
                            Criteria.builder()
                                .fieldName("productProcessTypes")
                                .operator("=")
                                .value(processType)
                                .build())
                        .build()))
            .build();

    ListCommonRequest listCommonRequest =
        ListCommonRequest.builder()
            .pageNo(1)
            .pageSize(Integer.MAX_VALUE)
            .filterCriteria(Arrays.asList(entityIdCriteria))
            .build();
    //        var repo = new ProductSequenceConfigRepository();
    //        var request = new ListRequest();
    //        request.Criteria = new Criteria("TenantProduct") == productId.Value && new
    // Criteria("ProductProcessTypes") == processType;
    //        return repo.List(uow.Connection, request).Entities.FirstOrDefault();
    return new ProductSequenceConfig();
  }

  public static String padLeft(String input, int len, char c) {
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

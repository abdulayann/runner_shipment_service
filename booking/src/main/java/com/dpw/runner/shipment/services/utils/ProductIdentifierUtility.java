package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.dao.impl.ProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITenantProductsDao;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Component
@Slf4j
public class ProductIdentifierUtility {

  //private List<TenantProducts> enabledTenantProducts = new ArrayList<>();

  @Autowired ITenantProductsDao tenantProductsDao;
  @Autowired ProductSequenceConfigDao productSequenceConfigDao;
  @Autowired IShipmentSettingsSync shipmentSettingsSync;
  @Autowired GetNextNumberHelper getNextNumberHelper;
  @Autowired V1AuthHelper v1AuthHelper;

  /**
   * Alternative for v1 constructor call with TenantSettings as a parameter
   *
   * @param shipmentSettingsDetails (tenant settings)
   */
  public List<TenantProducts> populateEnabledTenantProducts(ShipmentSettingsDetails shipmentSettingsDetails) {
     return mapTenantProducts(shipmentSettingsDetails);
  }

  private List<TenantProducts> mapTenantProducts(ShipmentSettingsDetails tenantSettings) {
    var tenantProducts = new ArrayList<TenantProducts>();
    var findProduct = fetchRegisteredProduct();
    for (var p : findProduct) {
      var product = new TenantProducts();
      product.setId(p.getId());
      product.setProductType(p.getProductType());
      product.setEnableGrouping(p.getEnableGrouping());
      tenantProducts.add(product);
    }
    return tenantProducts;
  }

  private List<TenantProducts> fetchRegisteredProduct() {
    ListCommonRequest listRequest = constructListCommonRequest("enabled", true, "=");
    Pair<Specification<TenantProducts>, Pageable> pair =
        fetchData(listRequest, TenantProducts.class);
    Page<TenantProducts> tenantProducts =
        tenantProductsDao.findAll(pair.getLeft(), pair.getRight());

    return tenantProducts.get().sorted(Comparator.comparing(TenantProducts::getPriority)).toList();
  }

  public String GetCommonSequenceNumber(
      String transportMode, ProductProcessTypes productProcessTypes) {
    var sequenceNumber = "";
    var productSequence = GetCommonProductSequence(transportMode, productProcessTypes);
    if (productSequence != null) {
      var regexPrefix = productSequence.getPrefix();
      log.info("CR-ID {} || prefix for common sequence {}", LoggerHelper.getRequestIdFromMDC(), regexPrefix);
      sequenceNumber = RegexToSequenceNumber(productSequence, transportMode);
    }
    return sequenceNumber;
  }

  private ProductSequenceConfig GetCommonProductSequence(
      String transportMode, ProductProcessTypes productProcessTypes) {
    ProductSequenceConfig returnProduct = null;
    ListCommonRequest listRequest =
        CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
    Pair<Specification<TenantProducts>, Pageable> pair =
        fetchData(listRequest, TenantProducts.class);
    Page<TenantProducts> tenantProducts =
        tenantProductsDao.findAll(pair.getLeft(), pair.getRight());
    List<TenantProducts> tenantProductList = tenantProducts.getContent();

    if (tenantProductList.size() > 0) {
      log.info("CR-ID {} || common sequence found", LoggerHelper.getRequestIdFromMDC());
      var tenantProductIds = tenantProductList.stream().map(TenantProducts::getId).toList();
      listRequest = getCommonProductSequenceListCriteria(tenantProductIds, productProcessTypes, transportMode);

      Map<String, RunnerEntityMapping> tableNames =
          Map.ofEntries(
              Map.entry("tenantProductId", RunnerEntityMapping.builder().tableName("tenantProducts").dataType(Long.class).fieldName("id").build()),
              Map.entry("transportMode", RunnerEntityMapping.builder().tableName("tenantProducts").dataType(List.class).fieldName("transportModes").build()),
              Map.entry(Constants.PRODUCT_PROCESS_TYPES, RunnerEntityMapping.builder().tableName("ProductSequenceConfig").dataType(ProductProcessTypes.class).fieldName(Constants.PRODUCT_PROCESS_TYPES).build())
          );
      log.info("CR-ID {} || retrieving product for common sequence", LoggerHelper.getRequestIdFromMDC());
      Pair<Specification<ProductSequenceConfig>, Pageable> productSequenceConfigPair =
          fetchData(listRequest, ProductSequenceConfig.class, tableNames);
        returnProduct = productSequenceConfigDao.findAndLock(productSequenceConfigPair.getLeft(), productSequenceConfigPair.getRight());
    }
    return returnProduct;
  }

  private String RegexToSequenceNumber(
      ProductSequenceConfig productSequence, String transportMode) {
    StringBuilder result = new StringBuilder();
    var regexValue = productSequence.getPrefix();

    //var regexPattern = "(?:\\w+)\\{(.*?);(.*?)\\}\\{(.*?);(.*?)\\}\\{(.*?);(.*?)\\}\\{(.*?);(.*?)\\}"; // original pattern @"\{(.*?)\}"
    var regexPattern = "(?:\\{([^{}]+)\\}|([^{}]+))";

    Pattern pattern = Pattern.compile(regexPattern);
    // Use a Matcher to find all matches
    Matcher matcher = pattern.matcher(regexValue);
    List<String> segments = new ArrayList<>();

    // Find all matches and add them to the list
    while (matcher.find()) {
      segments.add(matcher.group(1) != null ? matcher.group(1) : matcher.group(2));
    }
    // Filter out empty or whitespace-only segments
    segments = segments.stream().filter(s -> !s.trim().isEmpty()).toList();

    for (var segment : segments) {
      if (segment.contains(";")) {
        var splitArray = segment.split(";");
        var regexKey = splitArray[0];
        var format = splitArray[1];
        Integer numberOfCharactersToRetain = 0;
        switch (regexKey.toLowerCase()) {
          case "branchcode" -> {
            var user = UserContext.getUser();
            String tenantCode = user.getCode();
            if (format.contains("L")) {
              format = format.replace("L", "");
              numberOfCharactersToRetain = tryParse(format, numberOfCharactersToRetain);
              if (numberOfCharactersToRetain!= null)
                result =
                    (result == null ? new StringBuilder("null") : result)
                        .append(
                            tenantCode.length() >= numberOfCharactersToRetain
                                ? tenantCode.substring(
                                    tenantCode.length() - numberOfCharactersToRetain)
                                : tenantCode);

            } else {
              numberOfCharactersToRetain = tryParse(format, numberOfCharactersToRetain);
              if (numberOfCharactersToRetain != null) {
                result =
                    (result == null ? new StringBuilder("null") : result)
                        .append(
                            tenantCode.substring(
                                0, Math.min(numberOfCharactersToRetain, tenantCode.length())));
              }
            }
          }
          case "transportmode" -> {
            numberOfCharactersToRetain = tryParse(format, numberOfCharactersToRetain);
            if (numberOfCharactersToRetain != null) {
              result =
                  (result == null ? new StringBuilder("null") : result)
                      .append(
                          transportMode.substring(
                              0, Math.min(numberOfCharactersToRetain, transportMode.length())));
            }
          }
          case "date" -> result =
              Optional.of(
                      result
                          .toString()
                          .concat(DateTimeFormatter.ofPattern(format).format(LocalDateTime.now())))
                  .map(StringBuilder::new)
                  .orElse(null);
          case "seq" -> {
            productSequence.setSerialCounter(
                productSequence.getSerialCounter() == null
                    ? 1
                    : (productSequence.getSerialCounter() + 1));
            Integer numberOfDigits = 0;
            numberOfDigits = tryParse(format, numberOfCharactersToRetain);
            if (numberOfDigits != null) {
            } else {
              numberOfDigits = 3;
            }
            String counter =
                    getNextNumberHelper.padLeft(productSequence.getSerialCounter().toString(), numberOfDigits, '0');
            result = (result == null ? new StringBuilder("null") : result).append(counter);
            log.info("CR-ID {} || Calling event {} from RegexToSequenceNumber", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PRODUCT_SEQ_SAVE);


            productSequence = productSequenceConfigDao.save(productSequence);
            try {
              shipmentSettingsSync.syncProductSequence(productSequence, v1AuthHelper.getHeadersForDataSync());
            } catch (Exception e) {
              log.error("Error performing sync on shipment settings product sequence entity, {}", e);
            }
          }
          default -> {}
        }
      } else {
        result = (result == null ? new StringBuilder("null") : result).append(segment);
      }
    }
    return result == null ? null : result.toString();
  }

  public TenantProducts getDefaultShipmentProduct(List<TenantProducts> enabledTenantProducts) {
    Optional<TenantProducts> transportAll =
        enabledTenantProducts.stream()
            .filter(i -> i.getProductType() == ProductType.Transport_All).findFirst();
    return transportAll.orElse(null);
  }

  private Integer tryParse(String in, int out) {
    try {
      out = Integer.parseInt(in);
      return out;
    } catch (Exception e) {
      return null;
    }
  }

  private ListCommonRequest getCommonProductSequenceListCriteria(List<Long> tenantProductIds, ProductProcessTypes productProcessTypes, String transportMode) {
    FilterCriteria entityIdCriteria = FilterCriteria.builder()
        .innerFilter(Arrays.asList(FilterCriteria.builder()
                .criteria(Criteria.builder()
                    .fieldName("tenantProductId")
                    .operator("IN")
                    .value(tenantProductIds)
                    .build()).build(),
            FilterCriteria.builder()
                .logicOperator("AND")
                .criteria(Criteria.builder()
                    .fieldName("productProcessTypes")
                    .operator("=")
                    .value(productProcessTypes.getDescription())
                    .build())
                .build(),
            FilterCriteria.builder()
                .logicOperator("AND")
                .criteria(Criteria.builder()
                    .fieldName("transportMode")
                    .operator("CONTAINS")
                    .value(transportMode)
                    .build())
                .build()
        ))
        .build();

    return ListCommonRequest.builder()
        .pageNo(1)
        .pageSize(Integer.MAX_VALUE)
        .filterCriteria(Arrays.asList(entityIdCriteria))
        .build();

  }
}

package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.dao.impl.ProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITenantProductsDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SEA;
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
   */
  public List<TenantProducts> populateEnabledTenantProducts() {
     return mapTenantProducts();
  }

  private List<TenantProducts> mapTenantProducts() {
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

  public ProductSequenceConfig getShipmentProductWithOutContainerType(
      ShipmentDetails shipmentDetails, ProductProcessTypes processType, List<TenantProducts> tenantProducts) {
    ShipmentDetails shipmentsRow1 = new ShipmentDetails();
    shipmentsRow1.setTransportMode(shipmentDetails.getTransportMode());
    shipmentsRow1.setDirection(shipmentDetails.getDirection());
    shipmentsRow1.setShipmentType("null");
    TenantProducts identifiedProduct = this.IdentifyProduct(shipmentsRow1, tenantProducts);
    if (identifiedProduct == null) {
      return null;
    } else {
      return getNextNumberHelper.getProductSequence(identifiedProduct.getId(), processType);
    }
  }

  public TenantProducts IdentifyProduct(ConsolidationDetails consolidation, List<TenantProducts> enabledTenantProducts) {
    Optional<TenantProducts> res = Optional.empty();

    if (isConsolSea(consolidation, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()

              .filter(p -> p.getProductType() == ProductType.Consolidation_Sea_EXIM).findFirst();
    } else if (isConsolAir(consolidation, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()

              .filter(p -> p.getProductType() == ProductType.Consolidation_Air_EXIM) .findFirst();
    } else {
      res =
          enabledTenantProducts.stream()

              .filter(p -> p.getProductType() == ProductType.Consolidation_All).findFirst();
    }

    return res.orElse(null);
  }

  public TenantProducts IdentifyProduct(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    String[] allCargoTypeProducts = {
      "Shipment_Sea_EXP_BBK",
      "Shipment_Sea_EXP_BLK",
      "Shipment_Sea_EXP_FCL",
      "Shipment_Sea_EXP_LCL",
      "Shipment_Sea_EXP_Liquid",
      "Shipment_Sea_EXP_RORO",
      "Shipment_Sea_IMP_BBK",
      "Shipment_Sea_IMP_BLK",
      "Shipment_Sea_IMP_FCL",
      "Shipment_Sea_IMP_LCL",
      "Shipment_Sea_IMP_Liquid",
      "Shipment_Sea_IMP_RORO",
      "Shipment_Sea_TransShip_BBK",
      "Shipment_Sea_TransShip_BLK",
      "Shipment_Sea_TransShip_FCL",
      "Shipment_Sea_TransShip_LCL",
      "Shipment_Sea_TransShip_Liquid",
      "Shipment_Sea_TransShip_RORO",
      "Shipment_Sea_ReShip_BBK",
      "Shipment_Sea_ReShip_BLK",
      "Shipment_Sea_ReShip_FCL",
      "Shipment_Sea_ReShip_LCL",
      "Shipment_Sea_ReShip_Liquid",
      "Shipment_Sea_ReShip_RORO",
      "Shipment_Sea_CrossTrade_BBK",
      "Shipment_Sea_CrossTrade_BLK",
      "Shipment_Sea_CrossTrade_FCL",
      "Shipment_Sea_CrossTrade_LCL",
      "Shipment_Sea_CrossTrade_LQD",
      "Shipment_Sea_CrossTrade_ROR"
    };
    for (String type : allCargoTypeProducts) {
      var product = checkShipSeaCargoTypeProduct(shipment, type, enabledTenantProducts);
      if (product != null) return product;
    }

    var allProduct = checkShipAllCargoTypeProduct(shipment, enabledTenantProducts);
    if (allProduct != null) return allProduct;

    Optional<TenantProducts> res = Optional.empty();

    if (isShipmentAirIMP(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Air_IMP)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (isShipmentAirEXP(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Air_EXP)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (isShipmentAirCrossTrade(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Air_CrossTrade)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (isShipmentAirTransShip(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Air_TransShip)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (Objects.equals(shipment.getTransportMode(), AIR)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Air_EXIM)
              .findFirst();
      if (res.isPresent()) return res.get();
    }

    if (isShipmentSeaIMP(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Sea_IMP)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (isShipmentSeaEXP(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Sea_EXP)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (isShipmentSeaCrossTrade(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Sea_CrossTrade)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (isShipmentSeaTransShip(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Sea_TransShip)
              .findFirst();
      if (res.isPresent()) return res.get();
    } else if (Objects.equals(shipment.getTransportMode(), SEA)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Sea_EXIM)
              .findFirst();
      if (res.isPresent()) return res.get();
    }

    if (isShipmentRoadEXP(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Road_EXP)
              .findFirst();
      if (res.isPresent()) return res.get();
    }
    if (isShipmentRoadCrossTrade(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Road_CrossTrade)
              .findFirst();
      if (res.isPresent()) return res.get();
    }

    if (isShipmentRoadTransShip(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Road_TransShip)
              .findFirst();
      if (res.isPresent()) return res.get();
    }

    if (isShipmentRailCrossTrade(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Rail_CrossTrade)
              .findFirst();
      if (res.isPresent()) return res.get();
    }

    if (isShipmentRailTransShip(shipment, enabledTenantProducts)) {
      res =
          enabledTenantProducts.stream()
              .filter(p -> p.getProductType() == ProductType.Shipment_Rail_TransShip)
              .findFirst();
      if (res.isPresent()) return res.get();
    }

    var transportAll =
        enabledTenantProducts.stream()
            .filter(p -> p.getProductType() == ProductType.Transport_All)
            .findFirst();
    return transportAll.orElse(null);
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

  TenantProducts checkShipSeaCargoTypeProduct(ShipmentDetails shipment, String type, List<TenantProducts> enabledTenantProducts) {
    boolean ans = isSea(shipment.getTransportMode());
    if (type.contains("IMP")) ans = ans && isImport(shipment.getDirection());
    else if (type.contains("EXP")) ans = ans && isExport(shipment.getDirection());
    else if (type.contains("TransShip")) ans = ans && isTransShip(shipment.getDirection());
    else if (type.contains("CrossTrade")) ans = ans && isCrossTrade(shipment.getDirection());

    ans = ans && type.contains(shipment.getShipmentType());
    if (ans) {
      Optional<TenantProducts> optional =
          enabledTenantProducts.stream()
              .filter(
                  i -> {
                    try {
                      ProductType p = ProductType.valueOf(type);
                      return i.getProductType() == p;
                    } catch (Exception ignored) {
                      return false;
                    }
                  }).findFirst();
      if (optional.isPresent()) {
        return optional.get();
      }
    }
    return null;
  }

  TenantProducts checkShipAllCargoTypeProduct(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    TenantProducts product = null;
    List<String> allCargoTypeProducts = new ArrayList<>();
    if (isAir(shipment.getTransportMode()))
      allCargoTypeProducts = List.of("Shipment_Air_CrossTrade_LSE", "Shipment_Air_CrossTrade_ULD");
    else if (isRoad(shipment.getTransportMode()))
      allCargoTypeProducts =
          List.of(
              "Shipment_Road_CrossTrade_BBK",
              "Shipment_Road_CrossTrade_BLK",
              "Shipment_Road_CrossTrade_FCL",
              "Shipment_Road_CrossTrade_LCL",
              "Shipment_Road_CrossTrade_LQD",
              "Shipment_Road_CrossTrade_ROR");
    else if (isRail(shipment.getTransportMode()))
      allCargoTypeProducts =
          List.of(
              "Shipment_Rail_CrossTrade_BBK",
              "Shipment_Rail_CrossTrade_BLK",
              "Shipment_Rail_CrossTrade_FCL",
              "Shipment_Rail_CrossTrade_LCL",
              "Shipment_Rail_CrossTrade_LQD",
              "Shipment_Rail_CrossTrade_ROR");

    for (var type : allCargoTypeProducts) {
      boolean ans = true;
      if (type.contains("IMP")) ans = ans && isImport(shipment.getDirection());
      else if (type.contains("EXP")) ans = ans && isExport(shipment.getDirection());
      else if (type.contains("TransShip")) ans = ans && isTransShip(shipment.getDirection());
      else if (type.contains("CrossTrade")) ans = ans && isCrossTrade(shipment.getDirection());

      if (type.contains(shipment.getShipmentType()) && ans) {
        Optional<TenantProducts> optional =
            enabledTenantProducts.stream()
                .filter(
                    i -> {
                      try {
                        ProductType p = ProductType.valueOf(type);
                        return i.getProductType() == p;
                      } catch (Exception ignored) {
                        return false;
                      }
                    })
                .findFirst();
        if (optional.isPresent()) {
          product = optional.get();
          break;
        }
      }
    }
    return product;
  }

  public TenantProducts getDefaultShipmentProduct(List<TenantProducts> enabledTenantProducts) {
    Optional<TenantProducts> transportAll =
        enabledTenantProducts.stream()
            .filter(i -> i.getProductType() == ProductType.Transport_All).findFirst();
    return transportAll.orElse(null);
  }

  boolean isShipmentRoadEXP(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isRoad(shipment.getTransportMode())
        && isExport(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Road_EXP);
  }

  boolean isShipmentRoadCrossTrade(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isRoad(shipment.getTransportMode())
        && isCrossTrade(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Road_CrossTrade);
  }

  boolean isShipmentRoadTransShip(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isRoad(shipment.getTransportMode())
        && isTransShip(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Road_TransShip);
  }

  boolean isShipmentRailCrossTrade(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isRail(shipment.getTransportMode())
        && isCrossTrade(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Rail_CrossTrade);
  }

  boolean isShipmentRailTransShip(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isRail(shipment.getTransportMode())
        && isTransShip(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Rail_TransShip);
  }

  boolean isConsolAir(ConsolidationDetails consolidationRow, List<TenantProducts> enabledTenantProducts) {
    return isAir(consolidationRow.getTransportMode())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Consolidation_Air_EXIM);
  }

  boolean isConsolSea(ConsolidationDetails consolidationRow, List<TenantProducts> enabledTenantProducts) {
    return isSea(consolidationRow.getTransportMode())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Consolidation_Sea_EXIM);
  }

  boolean isAir(String mode) {
    return mode.equalsIgnoreCase("Air");
  }

  boolean isSea(String mode) {
    return mode.equalsIgnoreCase("Sea");
  }

  boolean isRoad(String mode) {
    return mode.equalsIgnoreCase("ROAD")
        || mode.equalsIgnoreCase("ROA")
        || mode.equalsIgnoreCase("RF");
  }

  boolean isRail(String mode) {
    return mode.equalsIgnoreCase("RAI");
  }

  boolean isExport(String mode) {
    return mode.equalsIgnoreCase("EXP");
  }

  boolean isImport(String mode) {
    return mode.equalsIgnoreCase("IMP");
  }

  boolean isTransShip(String mode) {
    return mode.equalsIgnoreCase("TRA");
  }

  boolean isCrossTrade(String mode) {
    return mode.equalsIgnoreCase("CTS");
  }

  boolean isShipmentAirIMP(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isAir(shipment.getTransportMode())
        && isImport(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Air_IMP);
  }

  boolean isShipmentAirEXP(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isAir(shipment.getTransportMode())
        && isExport(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Air_EXP);
  }

  boolean isShipmentAirCrossTrade(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isAir(shipment.getTransportMode())
        && isCrossTrade(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Air_CrossTrade);
  }

  boolean isShipmentAirTransShip(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isAir(shipment.getTransportMode())
        && isTransShip(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Air_TransShip);
  }

  boolean isShipmentSeaIMP(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isSea(shipment.getTransportMode())
        && isImport(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Sea_IMP);
  }

  boolean isShipmentSeaEXP(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isSea(shipment.getTransportMode())
        && isExport(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Sea_EXP);
  }

  boolean isShipmentSeaCrossTrade(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isSea(shipment.getTransportMode())
        && isCrossTrade(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Sea_CrossTrade);
  }

  boolean isShipmentSeaTransShip(ShipmentDetails shipment, List<TenantProducts> enabledTenantProducts) {
    return isSea(shipment.getTransportMode())
        && isTransShip(shipment.getDirection())
        && enabledTenantProducts.stream()
            .anyMatch(p -> p.getProductType() == ProductType.Shipment_Sea_TransShip);
  }

  private Integer tryParse(String in, int out) {
    try {
      out = Integer.parseInt(in);
      return out;
    } catch (Exception e) {
      return null;
    }
  }

  public String getCustomizedBLNumber(ShipmentDetails shipmentDetails) throws RunnerException {
    List<TenantProducts> enabledTenantProducts = this.populateEnabledTenantProducts();

    TenantProducts identifiedProduct = this.IdentifyProduct(shipmentDetails, enabledTenantProducts);
    if (identifiedProduct == null){
      if(!shipmentDetails.getTransportMode().equalsIgnoreCase("Air")){
        // to check the commmon sequence
        String sequenceNumber = GetChildCommonSequenceNumber(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentId(), ProductProcessTypes.HBLNumber);
        if (StringUtility.isNotEmpty(sequenceNumber)) {
          return sequenceNumber;
        }
      } else {
        String hawbSequenceNumber = GetChildCommonSequenceNumber(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentId(), ProductProcessTypes.HAWB);
        if (StringUtility.isNotEmpty(hawbSequenceNumber)) {
          return hawbSequenceNumber;
        }
      }
      return "";
    }
    ProductProcessTypes processType;
    if(shipmentDetails.getTransportMode().equalsIgnoreCase("Air")) {
      processType = ProductProcessTypes.HAWB;
      String sequenceNumber = GetChildCommonSequenceNumber(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentId(), processType);
      if (StringUtility.isNotEmpty(sequenceNumber)) {
        return sequenceNumber;
      }
    }
    else
    {
      processType = ProductProcessTypes.HBLNumber;
      // to check the commmon sequence
      String sequenceNumber = GetChildCommonSequenceNumber(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentId(), processType);
      if (StringUtility.isNotEmpty(sequenceNumber)) {
        return sequenceNumber;
      }
    }
    ProductSequenceConfig sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), processType);
    if(sequenceSettings == null){
      sequenceSettings = getShipmentProductWithOutContainerType(shipmentDetails, processType, enabledTenantProducts);
      if (sequenceSettings == null)
      {
        return "";
      }
    }
    String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
    return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, UserContext.getUser().TenantId, true, null, false);
  }

  public String GetChildCommonSequenceNumber(String transportMode, String parentNumber, ProductProcessTypes productProcessTypes) {
    String sequenceNumber = "";
    ProductSequenceConfig productSequence = GetCommonProductSequence(transportMode, productProcessTypes);
    if (productSequence != null) {
      sequenceNumber = parentNumber;
      if (productProcessTypes == ProductProcessTypes.HBLNumber || productProcessTypes == ProductProcessTypes.HAWB) {
        return sequenceNumber;
      }
    }
    return sequenceNumber;
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

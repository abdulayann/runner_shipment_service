package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ContainerCountByCode;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentAndContainerResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbSpecialHandlingCodesMappingInfo;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchMultiLangChargeCodeRequest;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import com.dpw.runner.shipment.services.dto.v1.request.AddressTranslationRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.DigitGrouping;
import com.dpw.runner.shipment.services.entity.enums.GroupingNumber;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.TranslationException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.request.ShipmentGuidRequest;
import com.dpw.runner.shipment.services.masterdata.response.*;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Slf4j
@SuppressWarnings("unchecked")
public abstract class IReport {


    public static final String LOCAL_NAME = "LocalName";
    public static final String REGEX_S_S = "%s %s";
    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IHblDao hblDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAwbRepository awbRepository;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private  IAwbDao awbDao;
    @Autowired
    private INPMServiceAdapter npmServiceAdapter;

    @Autowired
    MasterDataFactory masterDataFactory;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    CacheManager cacheManager;
    @Autowired
    public MasterDataUtils masterDataUtils;
    @Autowired
    CustomKeyGenerator keyGenerator;

    @Autowired
    private IContainerService containerService;
    @Autowired
    private IAwbService awbService;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private IPackingService packingService;

    @Autowired
    private CommonUtils commonUtils;


    public abstract Map<String, Object> getData(Long id) throws RunnerException;
    abstract IDocumentModel getDocumentModel(Long id) throws RunnerException;
    abstract Map<String, Object> populateDictionary(IDocumentModel documentModel);

    public ShipmentContainers getShipmentContainer(ContainerModel row)
    {
        ShipmentContainers ship = new ShipmentContainers();
        ship.ContainerNumber = row.getContainerNumber();
        ship.SealNumber = StringUtility.isEmpty(row.getCarrierSealNumber()) ? row.getShipperSealNumber() : row.getCarrierSealNumber();
        ship.NoofPackages = IsStringNullOrEmpty(row.getPacks()) ? null : Long.valueOf(row.getPacks());
        if(row.getPacks() != null && !row.getPacks().isEmpty())
            ship.ShipmentPacks = Long.valueOf(row.getPacks());
        ship.ShipmentPacksUnit = row.getPacksType();
        ship.GrossWeight = row.getGrossWeight();
        ship.GrossWeightUnit = row.getGrossWeightUnit();
        ship.TareWeight =  row.getTareWeight();
        ship.TareWeightUnit = row.getTareWeightUnit();
        ship.Measurement = getRoundedBigDecimal(row.getMeasurement(),2, RoundingMode.HALF_UP);
        ship.MeasurementUnit = row.getMeasurementUnit();
        ship.GrossVolume = row.getGrossVolume();
        ship.GrossVolumeUnit = row.getGrossVolumeUnit();
        ship.ContainerTypeCode = row.getContainerCode();
        ship.ContainerCount = row.getContainerCount();
        ship.ShipmentMarksnNums = row.getMarksNums();
        ship.NetWeight = row.getNetWeight();
        ship.NetWeightUnit = row.getNetWeightUnit();
        ship.MinTemp = getRoundedBigDecimal(row.getMinTemp(),2, RoundingMode.HALF_UP);
        ship.MinTempUnit = row.getMinTempUnit();
        ship.ShipmentHblDeliveryMode = row.getHblDeliveryMode();
        ship.DescriptionOfGoods = row.getDescriptionOfGoods();
        ship.CarrierSealNumber = row.getCarrierSealNumber();
        ship.CustomsSealNumber = row.getCustomsSealNumber();
        ship.ShipperSealNumber = row.getShipperSealNumber();
        ship.HazardousUn = row.getHazardousUn();
        ship.CargoGrossWeightUnit = String.format("%s %s", ConvertToWeightNumberFormat(row.getGrossWeight(), TenantSettingsDetailsContext.getCurrentTenantSettings()), row.getGrossWeightUnit());

        try {
            List<MasterListRequest> requests = new ArrayList<>();
            Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);

            Cache.ValueWrapper value1 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, ship.GrossVolumeUnit));
            if(Objects.isNull(value1))
                requests.add(MasterListRequest.builder().ItemType(MasterDataType.VOLUME_UNIT.getDescription()).ItemValue(ship.GrossVolumeUnit).Cascade(null).build());

            Cache.ValueWrapper value2 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, ship.GrossWeightUnit));
            if(Objects.isNull(value2))
                requests.add(MasterListRequest.builder().ItemType(MasterDataType.WEIGHT_UNIT.getDescription()).ItemValue(ship.GrossWeightUnit).Cascade(null).build());

            Cache.ValueWrapper value3 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, ship.ShipmentPacksUnit));
            if(Objects.isNull(value3))
                requests.add(MasterListRequest.builder().ItemType(MasterDataType.PACKS_UNIT.getDescription()).ItemValue(ship.ShipmentPacksUnit).Cascade(null).build());

            requests.add(MasterListRequest.builder().ItemType(MasterDataType.DG_CLASS.getDescription()).ItemValue(row.getDgClass()).Cascade(null).build());

            if(requests.size() > 0) {
                MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
                masterListRequestV2.setMasterListRequests(requests);
                masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));
                Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
                masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

                // Populate DgClassDescription field from master data fetched
                String key = row.getDgClass() + '#' + MasterDataType.masterData(MasterDataType.DG_CLASS.getId()).name();
                ship.DgClassDescription = Optional.of(keyMasterDataMap.get(key)).map(i -> i.getValuenDesc()).orElse(null);
            }
            ship.VolumeUnitDescription = getMasterListItemDesc(ship.GrossVolumeUnit);
            ship.WeightUnitDescription = getMasterListItemDesc(ship.GrossWeightUnit);
            ship.PacksUnitDescription = getMasterListItemDesc(ship.ShipmentPacksUnit);
            if (row.getGrossWeight() != null && row.getTareWeight() != null)
                ship.VGMWeight = row.getGrossWeight().add(row.getTareWeight());
        } catch (Exception ignored) { }
        CommodityResponse commodityResponse = getCommodity(row.getCommodityCode());
        if (commodityResponse != null) {
            ship.CommodityDescription = commodityResponse.getDescription();
            ship.CommodityDescriptionWithHSCode = commodityResponse.getCommodityDescriptionWithHSCode();
        }
        if(row.getCommodityGroup() != null) {
            MasterData commodity = getMasterListData(MasterDataType.COMMODITY_GROUP, row.getCommodityGroup());
            if (commodity != null)
                ship.CommodityGroup = commodity.getItemDescription();
        }
        return ship;
    }

    private String getMasterListItemDesc(String value) {
        var valueMapper = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, value));
        if(!Objects.isNull(valueMapper)) {
            EntityTransferMasterLists object = (EntityTransferMasterLists) valueMapper.get();
            String val = value;
            if(!Objects.isNull(object) && object.getItemDescription() != null)
                val = object.getItemDescription();
            return val.toUpperCase();
        }
        return value;
    }

    public String getTotalPackUnitCode(List<ShipmentModel> shipmentModelList) {
        String unitCode = "";
        for (var c : shipmentModelList) {
            if (c.getPacksUnit() != null && !c.getPacksUnit().isEmpty()) {
                if (!unitCode.equals(c.getPacksUnit()))
                    unitCode = "MPK";
            } else {
                if (unitCode.equals("MPK"))
                    unitCode = c.getPacksUnit();
            }
        }
        return unitCode;
    }

    public void populateBLContainer(ShipmentContainers shipmentContainer, HblContainerDto blObjectContainer) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        Integer decimalPlaces = shipmentSettingsDetails.getDecimalPlaces();
        if(decimalPlaces == null)
            decimalPlaces = 2;
        shipmentContainer.BL_ContainerType = blObjectContainer.getContainerType();
        shipmentContainer.BL_SealNumber = blObjectContainer.getSealNumber();
        if (blObjectContainer.getContainerGrossWeight() != null)
            shipmentContainer.BL_GrossWeight = GetDPWWeightVolumeFormat(blObjectContainer.getContainerGrossWeight(), decimalPlaces, tenantSettings);
        else
            shipmentContainer.BL_GrossWeight = StringUtility.getEmptyString();
        shipmentContainer.BL_GrossWeightUnit = blObjectContainer.getContainerGrossWeightUnit();
        if (blObjectContainer.getContainerGrossVolume() != null)
            shipmentContainer.BL_GrossVolume = GetDPWWeightVolumeFormat(blObjectContainer.getContainerGrossVolume(), decimalPlaces, tenantSettings);
        else
            shipmentContainer.BL_GrossVolume = StringUtility.getEmptyString();
        shipmentContainer.BL_GrossVolumeUnit = blObjectContainer.getContainerGrossVolumeUnit();
        shipmentContainer.BL_NoofPackages = blObjectContainer.getNoOfPackages();
        shipmentContainer.BL_CarrierSealNumber = blObjectContainer.getCarrierSealNumber();
        shipmentContainer.BL_ContainerNumber = blObjectContainer.getContainerNumber();
        shipmentContainer.BL_ContainerDescription = blObjectContainer.getContainerDesc();
        shipmentContainer.BL_PackageUnit = blObjectContainer.getPackageUnit();
    }

    public void populateShipmentFields(ShipmentModel shipment, Map<String, Object> dictionary)
    {
        if (shipment == null) {
            return;
        }
        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        PickupDeliveryDetailsModel pickup = shipment.getPickupDetails();
        PickupDeliveryDetailsModel delivery = shipment.getDeliveryDetails();
        if(shipment.getTransportInstructionId() != null)
            addTransportInstructionTags(dictionary , shipment);
        PartiesModel shipmentClient = shipment.getClient();
        PartiesModel shipmentConsignee = shipment.getConsignee();
        PartiesModel shipmentConsigner = shipment.getConsigner();
        AdditionalDetailModel additionalDetails = new AdditionalDetailModel();
        if(shipment.getAdditionalDetails() != null) {
            additionalDetails = shipment.getAdditionalDetails();
        }
        // UnLocations Master-data
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(shipment);
        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
        // Master lists Master-data
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromShipment(shipment);
        masterListRequest.addAll(createMasterListsRequestFromUnLocoMap(unlocationsMap));
        Map<Integer, Map<String, MasterData>> masterListsMap = fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).toList()).build());
        PartiesModel shipmentNotify = additionalDetails.getNotifyParty();

        UnlocationsResponse pol = unlocationsMap.get(shipment.getCarrierDetails().getOriginPort());
        UnlocationsResponse pod = unlocationsMap.get(shipment.getCarrierDetails().getDestinationPort());
        UnlocationsResponse origin = unlocationsMap.get(shipment.getCarrierDetails().getOrigin());
        UnlocationsResponse destination = unlocationsMap.get(shipment.getCarrierDetails().getDestination());
        UnlocationsResponse paidPlace = unlocationsMap.get(shipment.getAdditionalDetails().getPaidPlace());
        UnlocationsResponse placeOfIssue = unlocationsMap.get(shipment.getAdditionalDetails().getPlaceOfIssue());
        Map<String, WareHouseResponse> wareHouseResponseMap = masterDataUtils.fetchWareHouseData(
                Arrays.asList(shipment.getAdditionalDetails().getWarehouseId(), shipment.getAdditionalDetails().getBondedWarehouseId()));
        dictionary.put(REFERENCE_NO, shipment.getBookingReference());
        dictionary.put(ReportConstants.MASTER_BILL,shipment.getMasterBill());
        dictionary.put(ReportConstants.HOUSE_BILL,shipment.getHouseBill());
        dictionary.put(SHIPMENT_ID, shipment.getShipmentId());
        VesselsResponse vesselsResponse = getVesselsData(shipment.getCarrierDetails().getVessel());
        if(Objects.equals(shipment.getTransportMode(), AIR))
        {
            dictionary.put(VesselsNameFlightName, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getShippingLine() : null);
        }
        if(vesselsResponse != null) {
            dictionary.put(VESSEL_NAME, vesselsResponse.getName());
            if(!Objects.equals(shipment.getTransportMode(), AIR))
                dictionary.put(VesselsNameFlightName, vesselsResponse.getName());
        }
        dictionary.put(ReportConstants.VOYAGE,shipment.getCarrierDetails().getVoyage());
        if(!Objects.isNull(pol)) dictionary.put(ReportConstants.POL_CODE, pol.getLocCode());
        if(!Objects.isNull(pod)) dictionary.put(ReportConstants.POD_CODE, pod.getLocCode());
        dictionary.put(ReportConstants.POD_COUNTRY, pod != null ? pod.getCountry() : null);
        dictionary.put(ReportConstants.POL_COUNTRY, pol != null ? pol.getCountry() : null);
        dictionary.put(CARGO_TERMS_DESCRIPTION, StringUtility.toUpperCase(shipment.getGoodsDescription()));
        dictionary.put(BL_DESCRIPTION, StringUtility.toUpperCase(additionalDetails.getBLRemarksDescription()));
        dictionary.put(LOAD_DESCRIPTION_REMARKS, StringUtility.toUpperCase(additionalDetails.getBLRemarks()));
        if(pol != null) {
            if(pol.getPortName() != null) {
                dictionary.put(ReportConstants.POL_PORT_NAME_WITH_COUNTRY_IN_CAPS, (pol.getPortName().toUpperCase() + ", " + (masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(pol.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(pol.getCountry()).getItemDescription().toUpperCase() : "")));
                dictionary.put(ReportConstants.POL_IN_CAPS, pol.getPortName().toUpperCase());
                dictionary.put(ORIGIN_PORT, dictionary.get(POL_IN_CAPS));
            }
            else
                dictionary.put(ORIGIN_PORT, pol.getName());
            dictionary.put(POL_COUNTRY_NAME, pol.getCountryName());
            dictionary.put(POL_COUNTRY_NAME_IN_CAPS , (masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(pol.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(pol.getCountry()).getItemDescription().toUpperCase() : ""));
            dictionary.put(ReportConstants.POL_AIRPORT_CODE, pol.getIataCode());
            if(pol.getIataCode() != null) {
                dictionary.put(ReportConstants.POL_AIRPORT_CODE_IN_CAPS, pol.getIataCode().toUpperCase());
            }
            dictionary.put(POL_CODE_IN_CAPS, StringUtility.toUpperCase(pol.getLocCode()));
        }
        if(shipment.getAdditionalDetails() != null) {
            dictionary.put(ReportConstants.COUNTRY_OF_GOODS_ORIGIN, shipment.getAdditionalDetails().getGoodsCO());
        }
        dictionary.put(CONTAINER_SUMMARY, shipment.getSummary());
        dictionary.put(PACK_SUMMARY, shipment.getPackSummary());
        dictionary.put(ReportConstants.SERVICE_MODE, shipment.getServiceType());
        dictionary.put(ReportConstants.POL_PORTNAME, pol != null ? pol.getPortName() : null);
        dictionary.put(ReportConstants.POL_PORT_NAME_IN_CAPS, pol != null ? StringUtility.toUpperCase(pol.getPortName()) : null);
        dictionary.put(ReportConstants.POD_PORTNAME, pod != null ? pod.getPortName() : null);
        dictionary.put(ReportConstants.POD_PORT_NAME_IN_CAPS, pod != null ? StringUtility.toUpperCase(pod.getPortName()) : null);

        if (pod != null) {
            String podCountry = masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(pod.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(pod.getCountry()).getItemDescription() : "";
            dictionary.put(ReportConstants.DESTINATION_AIRPORT_COUNTRY, podCountry.toUpperCase());
            if (pod.getPortName() != null) {
                dictionary.put(ReportConstants.POD_PORT_NAME_WITH_COUNTRY_IN_CAPS, pod.getPortName().toUpperCase() + ", " + podCountry.toUpperCase());
                dictionary.put(ReportConstants.POD_IN_CAPS, pod.getPortName().toUpperCase());
                dictionary.put(DESTINATION_PORT, dictionary.get(POD_IN_CAPS));
            }
            else
                dictionary.put(DESTINATION_PORT, pod.getName());
            dictionary.put(POD_COUNTRY_NAME, pod.getCountryName());
            dictionary.put(ReportConstants.POD_COUNTRY_NAME_IN_CAPS, podCountry.toUpperCase());
            dictionary.put(ReportConstants.POD_AIRPORT_CODE, pod.getIataCode());
            if (pod.getIataCode() != null) {
                dictionary.put(ReportConstants.POD_AIRPORT_CODE_IN_CAPS, pod.getIataCode().toUpperCase());
            }
            if (pod.getPortName() != null)
                dictionary.put(ReportConstants.POD_IN_CAPS, pod.getPortName().toUpperCase());
            dictionary.put(POD_CODE_IN_CAPS, StringUtility.toUpperCase(pod.getLocCode()));
        }

        dictionary.put(ReportConstants.CARRIER, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getShippingLine() : null);
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, pod != null ? pod.getName() : null);
        dictionary.put(ReportConstants.PORT_OF_LOADING, pol != null ? pol.getName() : null);
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.REFERENCE_NUMBER,shipment.getBookingReference());
        dictionary.put(ReportConstants.ORIGIN_NAME, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.ORIGIN, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.ORIGIN_COUNTRY, origin != null ? origin.getCountry() : null);
        if(origin != null) {
            dictionary.put(ReportConstants.ORIGIN_NAME_IN_CAPS, origin.getName().toUpperCase());
            String originCountry = masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(origin.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(origin.getCountry()).getItemDescription() : "";
            dictionary.put(ReportConstants.ORIGIN_COUNTRY_NAME_IN_CAPS, originCountry.toUpperCase());
            dictionary.put(ReportConstants.POR_IN_CAPS, origin.getName().toUpperCase());
            dictionary.put(ReportConstants.POR_COUNTRY_NAME_IN_CAPS, originCountry.toUpperCase());
            dictionary.put(ORIGIN_CODE_IN_CAPS, StringUtility.toUpperCase(origin.getLocCode()));
        }
        dictionary.put(ReportConstants.DESCRIPTION,shipment.getGoodsDescription());
        dictionary.put(ReportConstants.SHIPMENT_TYPE,shipment.getDirection());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, shipment.getDirection() != null ? Character.toUpperCase(shipment.getDirection().charAt(0)) : null);
        int containerCount = 0;
        if (shipment.getContainersList().size() > 0) {
            for (ContainerModel container : shipment.getContainersList()) {
                if (container.getContainerCount() != null && container.getContainerCount() != 0) {
                    containerCount += container.getContainerCount().intValue();
                }
            }
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT, numberToWords(containerCount).toUpperCase());
        dictionary.put(PICKUP_INSTRUCTION, shipment.getPickupDetails() != null ? shipment.getPickupDetails().getPickupDeliveryInstruction() : null);
        dictionary.put(DELIVERY_INSTRUCTIONS, shipment.getDeliveryDetails() != null ? shipment.getDeliveryDetails().getPickupDeliveryInstruction() : null);
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.ETA, ConvertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEta() : null, tsDateTimeFormat));
        dictionary.put(ReportConstants.ETD, ConvertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEtd() : null, tsDateTimeFormat));
        dictionary.put(ReportConstants.ATA, ConvertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAta() : null, tsDateTimeFormat));
        dictionary.put(ReportConstants.ATD, ConvertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAtd() : null, tsDateTimeFormat));
        dictionary.put(ReportConstants.DATE_OF_DEPARTURE, dictionary.get(ReportConstants.ATD) == null ? dictionary.get(ReportConstants.ETD) : dictionary.get(ReportConstants.ATD));
        dictionary.put(ReportConstants.SYSTEM_DATE, ConvertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat));
        dictionary.put(ReportConstants.ONBOARD_DATE, ConvertToDPWDateFormat(additionalDetails.getOnBoardDate(), tsDateTimeFormat));
        dictionary.put(ReportConstants.ESTIMATED_READY_FOR_PICKUP, pickup != null ? pickup.getEstimatedPickupOrDelivery() : null);
        String formatPattern = "dd/MMM/y";
        if(!CommonUtils.IsStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
            formatPattern = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.SHIPMENT_CREATION_DATE, ConvertToDPWDateFormat(shipment.getShipmentCreatedOn(), tsDateTimeFormat));
        dictionary.put(ReportConstants.DATE_OF_ISSUE, ConvertToDPWDateFormat(additionalDetails.getDateOfIssue(), formatPattern, true));
        dictionary.put(SHIPMENT_DETAIL_DATE_OF_ISSUE, ConvertToDPWDateFormat(additionalDetails.getDateOfIssue(), formatPattern, true));
        dictionary.put(ReportConstants.DATE_OF_RECEIPT, additionalDetails.getDateOfReceipt());

        dictionary.put(ReportConstants.INCO_TERM, shipment.getIncoterms());
        dictionary.put(ReportConstants.INCOTERM, shipment.getIncoterms());
        dictionary.put(ReportConstants.CHARGEABLE, ConvertToWeightNumberFormat(shipment.getChargable(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CHARGEABLE_UNIT, shipment.getChargeableUnit());
        dictionary.put(ReportConstants.TRANSPORT_MODE, shipment.getTransportMode());
        dictionary.put(ReportConstants.ContainerType, shipment.getShipmentType());

        MasterData masterData = null;
        if (masterListsMap.containsKey(MasterDataType.TRANSPORT_MODE.getId()) && masterListsMap.get(MasterDataType.TRANSPORT_MODE.getId()).containsKey(shipment.getTransportMode()))
            masterData = masterListsMap.get(MasterDataType.TRANSPORT_MODE.getId()).get(shipment.getTransportMode());
        dictionary.put(ReportConstants.TRANSPORT_MODE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getTransportMode());
        if (masterListsMap.containsKey(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId()) && masterListsMap.get(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId()).containsKey(shipment.getDirection()))
            masterData = masterListsMap.get(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId()).get(shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_TYPE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_NUMBER, shipment.getShipmentId());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, shipment.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.ADDITIONAL_TERMS, shipment.getAdditionalTerms());

        dictionary.put(ReportConstants.PACKS, GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks() != null ? shipment.getNoOfPacks() : 0), 0, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.PACKS_UNIT,Constants.MPK.equals(shipment.getPacksUnit()) ? Constants.PACKAGES : shipment.getPacksUnit());
        dictionary.put(ReportConstants.PACKS_WITH_COMMA, addCommas(shipment.getNoOfPacks()));
        if (masterListsMap.containsKey(MasterDataType.PACKS_UNIT.getId()) && masterListsMap.get(MasterDataType.PACKS_UNIT.getId()).containsKey(shipment.getPacksUnit()))
            masterData = masterListsMap.get(MasterDataType.PACKS_UNIT.getId()).get(shipment.getPacksUnit());
        dictionary.put(ReportConstants.PACKS_UNIT_DESC, masterData != null && StringUtility.isNotEmpty(masterData.getItemDescription()) ? masterData.getItemDescription() : shipment.getPacksUnit());

        if(shipment.getInnerPacks() != null)
            dictionary.put(ReportConstants.INNERS, GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getInnerPacks()), 0, v1TenantSettingsResponse));

        dictionary.put(ReportConstants.PAYMENT_TERMS , shipment.getPaymentTerms() == null ?"" :shipment.getPaymentTerms());

        if (masterListsMap.containsKey(MasterDataType.PAYMENT.getId()) && masterListsMap.get(MasterDataType.PAYMENT.getId()).containsKey(shipment.getPaymentTerms()))
            masterData = masterListsMap.get(MasterDataType.PAYMENT.getId()).get(shipment.getPaymentTerms());
        if(masterData != null) {
            dictionary.put(ReportConstants.PAYMENT_TERMS_DESCRIPTION, masterData.getItemDescription());
        }

        if (masterListsMap.containsKey(MasterDataType.SERVICE_MODE.getId()) && masterListsMap.get(MasterDataType.SERVICE_MODE.getId()).containsKey(shipment.getServiceType())) {
            masterData = masterListsMap.get(MasterDataType.SERVICE_MODE.getId()).get(shipment.getServiceType());
            dictionary.put(ReportConstants.SERVICE_MODE_DESCRIPTION, StringUtility.isNotEmpty(masterData.getItemDescription()) ? StringUtility.toUpperCase(masterData.getItemDescription()) : shipment.getServiceType());
        }

        dictionary.put(ReportConstants.GROSS_WEIGHT, ConvertToWeightNumberFormat(shipment.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GROSS_WEIGHT_UNIT,shipment.getWeightUnit());
        dictionary.put(ReportConstants.GROSS_VOLUME, ConvertToVolumeNumberFormat(shipment.getVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GROSS_VOLUME_UNIT,shipment.getVolumeUnit());
        dictionary.put(ReportConstants.GROSS_WEIGHT_WITH_COMMA, ConvertToWeightNumberFormat(shipment.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GROSS_VOLUME_WITH_COMMA, ConvertToVolumeNumberFormat(shipment.getVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.VOLUME_WEIGHT_WITH_COMMA, addCommas(shipment.getVolumetricWeight()));
        dictionary.put(ReportConstants.WEIGHT_UNIT_DESCRIPTION,  shipment.getNetWeightUnit());
        dictionary.put(ReportConstants.CHARGEABLE_WEIGHT, ConvertToVolumeNumberFormat(shipment.getChargable(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CHARGEABLE_WEIGHT_UNIT, shipment.getChargeableUnit());
        dictionary.put(ReportConstants.WEIGHTS, ConvertToWeightNumberFormat(shipment.getNetWeight(), v1TenantSettingsResponse));
        dictionary.put(WEIGHT, ConvertToWeightNumberFormat(shipment.getWeight(), v1TenantSettingsResponse));
        dictionary.put(VOLUME, ConvertToVolumeNumberFormat(shipment.getVolume(), v1TenantSettingsResponse));
        if (shipment.getVolumeUnit() != null && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getId())) {
            masterData =  masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).get(shipment.getVolumeUnit());
            dictionary.put(VolumeUnitDescription, masterData != null && masterData.getItemDescription() != null ? StringUtility.toUpperCase(masterData.getItemDescription()) : shipment.getVolumeUnit());
        }
        dictionary.put(NET_WEIGHT, addCommas(shipment.getNetWeight()));
        dictionary.put(NetWeight_Unit_Description, shipment.getNetWeightUnit());
        if (shipment.getNetWeightUnit() != null && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getId()) && masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).containsKey(shipment.getNetWeightUnit())) {
            masterData = masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).get(shipment.getNetWeightUnit());
            dictionary.put(NetWeight_Unit_Description, (masterData != null && masterData.getItemDescription() != null) ? StringUtility.toUpperCase(masterData.getItemDescription()) : shipment.getVolumeUnit());
        }

        dictionary.put(ReportConstants.DELIVERY_CFS, (delivery != null && !Objects.isNull(delivery.getSourceDetail()) && !Objects.isNull(delivery.getSourceDetail().getOrgData())) ? delivery.getSourceDetail().getOrgData().get(FULL_NAME) : null);
        dictionary.put(ReportConstants.PICKUP_CFS, (pickup != null && !Objects.isNull(pickup.getDestinationDetail()) && !Objects.isNull(pickup.getDestinationDetail().getOrgData()) )? pickup.getDestinationDetail().getOrgData().get(FULL_NAME) : null);
        dictionary.put(ReportConstants.MARKS_N_NUMS,shipment.getMarksNum());
        dictionary.put(ReportConstants.ORIGINALS,additionalDetails.getOriginal() == null ? 1 : additionalDetails.getOriginal());
        dictionary.put(ReportConstants.ORIGINAL_WORDS, numberToWords(additionalDetails.getOriginal() == null ? 1 : additionalDetails.getOriginal()));
        dictionary.put(ReportConstants.COPY_BILLS,additionalDetails.getCopy() == null ? 0 : additionalDetails.getCopy());

        dictionary.put(ReportConstants.ISSUE_PLACE_NAME, placeOfIssue !=  null ? placeOfIssue.getName() : null);
        dictionary.put(ReportConstants.ISSUE_PLACE_COUNTRY, placeOfIssue != null ? placeOfIssue.getCountry() : null);
        dictionary.put(ReportConstants.PAID_PLACE_NAME, paidPlace != null ? paidPlace.getName() : null);
        dictionary.put(ReportConstants.PAID_PLACE_COUNTRY, paidPlace != null ? paidPlace.getCountry() : null);

        dictionary.put(ReportConstants.HSN_NUMBER, additionalDetails.getHsnNumber());
        dictionary.put(ReportConstants.SHIPMENT_BOOKING_NUMBER, shipment.getBookingNumber());

        dictionary.put(ReportConstants.DESTINATION_NAME_, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.DESTINATION, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.DESTINATION_COUNTRY, destination != null ? destination.getCountry() : null);
        dictionary.put(DESTINATION_COUNTRY_NAME, destination != null ? destination.getCountryThreeDigitCode() : null);

        dictionary.put(ReportConstants.PRINT_DATE, ConvertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat));
        if(destination != null) {
            dictionary.put(ReportConstants.DESTINATION_NAME_IN_CAPS, destination.getName().toUpperCase());
            String destinationCountry = masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(destination.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(destination.getCountry()).getItemDescription() : "";
            dictionary.put(ReportConstants.DESTINATION_COUNTRY_NAME_IN_CAPS, destinationCountry.toUpperCase());
            dictionary.put(ReportConstants.FPOD_IN_CAPS, destination.getName().toUpperCase());
            dictionary.put(ReportConstants.FPOD_COUNTRY_NAME_IN_CAPS, destinationCountry.toUpperCase());
            dictionary.put(DESTINATION_CODE_IN_CAPS, StringUtility.toUpperCase(destination.getLocCode()));
        }

        if (Objects.equals(shipment.getPaymentTerms(), "PPD")) {
            dictionary.put(ReportConstants.AT, pol != null ? pol.getName() : null);
        } else if (Objects.equals(shipment.getPaymentTerms(), "CCX")) {
            dictionary.put(ReportConstants.AT, pod != null ? pod.getName() : null);
        }
        dictionary.put(PaymentTermsDescription,  shipment.getPaymentTerms());
        if (shipment.getPaymentTerms() != null && masterListsMap.containsKey(MasterDataType.PAYMENT.getId()) && masterListsMap.get(MasterDataType.PAYMENT.getId()).containsKey(shipment.getPaymentTerms())) {
            dictionary.put(PaymentTermsDescription,  StringUtility.toUpperCase(masterListsMap.get(MasterDataType.PAYMENT.getId()).get(shipment.getPaymentTerms()).getItemDescription()));
        }
        dictionary.put(ReportConstants.MARKS_N_NUMS_CAPS, StringUtility.toUpperCase(shipment.getMarksNum()));



        var array = new String[] {"" + dictionary.get("VesselName"), shipment.getCarrierDetails().getVoyage()};
        dictionary.put(ReportConstants.VESSEL_NAME_AND_VOYAGE, array[0] + " & " + array[1]);

        masterData = null;
        if(placeOfIssue != null && masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(placeOfIssue.getCountry()))  {
            masterData = masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(placeOfIssue.getCountry());
        }
        dictionary.put(ReportConstants.ISSUEPLACECOUNTRYNAME, masterData != null ? masterData.getItemDescription() : null);
        List<String> consignorFreeText = null, consigneeFreeText = null, notifyPartyFreeText = null;
        if ((Objects.equals(shipment.getTransportMode(), "SEA") || Objects.equals(shipment.getTransportMode(), "ROA") || Objects.equals(shipment.getTransportMode(), "RF") || Objects.equals(shipment.getTransportMode(), "AIR"))) {
            List<String> consigner = null;
            final String email = "Email";
            if(shipmentConsigner != null)
            {
                Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
                if(consignerAddress != null)
                {
                    consigner = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consignerAddress, COMPANY_NAME), getValueFromMap(consignerAddress, ADDRESS1),
                            getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                            getValueFromMap(consignerAddress, email), getValueFromMap(consignerAddress, CONTACT_PHONE),
                            getValueFromMap(consignerAddress,ZIP_POST_CODE));
                    dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get(COMPANY_NAME));
                    dictionary.put(ReportConstants.CONSIGNER_COMPANY_NAME, consignerAddress.get(COMPANY_NAME));
                    dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get(CONTACT_PERSON_ALIAS));
                    dictionary.put(ReportConstants.CONSIGNER_ADDRESS, ReportHelper.getOrgAddress(shipmentConsigner));

                    try {
                        dictionary.put(ReportConstants.ConsignerPhone, consignerAddress.get("ContactPhone"));
                        dictionary.put(ReportConstants.ConsignerFullName, shipmentConsigner.getOrgData().get(FULL_NAME1));
                    } catch (Exception ignored) { }
                }
                if(shipmentConsigner.getOrgData() != null)
                    dictionary.put(ReportConstants.CONSIGNER_LOCAL_NAME,shipmentConsigner.getOrgData().get(LOCAL_NAME));
                if (shipmentConsigner.getIsAddressFreeText() != null && shipmentConsigner.getIsAddressFreeText()) {
                    var rawData = consignerAddress != null && consignerAddress.containsKey(PartiesConstants.RAW_DATA) ? StringUtility.convertToString(consignerAddress.get(PartiesConstants.RAW_DATA)) : null;
                    consignorFreeText = ReportHelper.getAddressList(rawData);
                    dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consignorFreeText);
                    dictionary.put(ReportConstants.CONSIGNER_FREETEXTInCaps, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).toList());
                    dictionary.put(ReportConstants.CONSIGNER_NAME_FREETEXT_INCAPS, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).toList());
                } else {
                    dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
                }
            }
            List<String> consignee = populateConsigneeData(dictionary, shipmentConsignee);

            List<String> notify = null;
            if(shipmentNotify != null)
            {
                Map<String, Object> notifyAddress = shipmentNotify.getAddressData();
                if(notifyAddress != null)
                {
                    notify = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(notifyAddress, COMPANY_NAME), getValueFromMap(notifyAddress, ADDRESS1),
                            getValueFromMap(notifyAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(notifyAddress, CITY), getValueFromMap(notifyAddress, COUNTRY)),
                            getValueFromMap(notifyAddress, email), getValueFromMap(notifyAddress, CONTACT_PHONE),
                            getValueFromMap(notifyAddress,ZIP_POST_CODE)
                                                                     );
                    dictionary.put(ReportConstants.NOTIFY_PARTY_NAME,getValueFromMap(notifyAddress, COMPANY_NAME));
                    dictionary.put(ReportConstants.NOTIFY_PARTY_CONTACT_PERSON,getValueFromMap(notifyAddress, CONTACT_PERSON_ALIAS));
                }
                if(shipmentNotify.getOrgData() != null)
                    dictionary.put(ReportConstants.NOTIFY_PARTY_LOCAL_NAME,getValueFromMap(shipmentNotify.getOrgData(), LOCAL_NAME));
                if (shipmentNotify.getIsAddressFreeText() != null && shipmentNotify.getIsAddressFreeText()) {
                    var rawData = notifyAddress != null && notifyAddress.containsKey(PartiesConstants.RAW_DATA) ? StringUtility.convertToString(notifyAddress.get(PartiesConstants.RAW_DATA)) : null;
                    notifyPartyFreeText = ReportHelper.getAddressList(rawData);
                    dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notifyPartyFreeText);
                    dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT_IN_CAPS, notifyPartyFreeText == null ? null : notifyPartyFreeText.stream().map(StringUtility::toUpperCase).toList());
                    dictionary.put(ReportConstants.NOTIFY_PARTY_NAME_FREETEXT_INCAPS, notifyPartyFreeText == null || notifyPartyFreeText.isEmpty() ? null : StringUtility.toUpperCase(notifyPartyFreeText.get(0)));
                } else {
                    dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
                }

            }
            List<String> client = null;
            if(shipmentClient != null)
            {
                Map<String, Object> clientAddress = shipmentClient.getAddressData();
                if(clientAddress != null) {
                    client = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(clientAddress, COMPANY_NAME), getValueFromMap(clientAddress, ADDRESS1),
                            getValueFromMap(clientAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(clientAddress, CITY), getValueFromMap(clientAddress, COUNTRY)),
                            getValueFromMap(clientAddress, email), getValueFromMap(clientAddress, CONTACT_PHONE),
                            getValueFromMap(clientAddress, ZIP_POST_CODE));
                    dictionary.put(ReportConstants.CLIENT_NAME, getValueFromMap(clientAddress, COMPANY_NAME));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_1, getValueFromMap(clientAddress, ADDRESS1));
                    dictionary.put(CLIENT_ADDRESS_COUNTRY, getValueFromMap(clientAddress, COUNTRY));
                    dictionary.put(CLIENT_ADDRESS_CITY, getValueFromMap(clientAddress, CITY));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_PHONE, getValueFromMap(clientAddress, CONTACT_PHONE));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_MOBILE, getValueFromMap(clientAddress, "Mobile"));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_CONTACT_PERSON, getValueFromMap(clientAddress, CONTACT_PERSON_ALIAS));
                }
            }

            dictionary.put(ReportConstants.CONSIGNER,consigner);
            dictionary.put(ReportConstants.CONSIGNEE,consignee);
            dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
            dictionary.put(ReportConstants.CLIENT, client);

            PartiesModel notifyParty1 = null;
            List<PartiesModel> shipmentAddresses = shipment.getShipmentAddresses();
            for (PartiesModel party : shipmentAddresses) {
                if(Objects.equals(party.getType(), "Notify Party 1"))
                {
                    notifyParty1 = party;
                }
            }
            if(notifyParty1 != null){
                Map<String, Object> addressData = notifyParty1.getAddressData();
                List<String> notifyPartyAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                    StringUtility.convertToString(addressData.get(ADDRESS2)),
                    ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                    null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                    StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                );
                dictionary.put(SHIPMENT_NOTIFY_PARTY, notifyPartyAddress);
            }

            PartiesModel originAgent = additionalDetails.getExportBroker();
            if(originAgent != null) {
                Map<String, Object> addressData = originAgent.getAddressData();
                if(addressData != null) {
                    List<String> partyAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                            StringUtility.convertToString(addressData.get(ADDRESS2)),
                            ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                            null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                            StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                    );
                    dictionary.put(SHIPMENT_ORIGIN_AGENT, partyAddress);
                }
            }

            PartiesModel destinationAgent = additionalDetails.getImportBroker();
            if(destinationAgent != null) {
                Map<String, Object> addressData = destinationAgent.getAddressData();
                if(addressData != null) {
                    List<String> partyAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                            StringUtility.convertToString(addressData.get(ADDRESS2)),
                            ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                            null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                            StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                    );
                    dictionary.put(SHIPMENT_DESTINATION_AGENT, partyAddress);
                }
            }



            if(shipment.getPickupDetails() != null && shipment.getPickupDetails().getSourceDetail() != null)
            {
                Map<String, Object> pickupAddress = shipment.getPickupDetails().getSourceDetail().getAddressData();
                List<String> pickupAddressList;
                if(pickupAddress != null)
                {
                    pickupAddressList = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(pickupAddress, COMPANY_NAME), getValueFromMap(pickupAddress, ADDRESS1),
                            getValueFromMap(pickupAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(pickupAddress, CITY), getValueFromMap(pickupAddress, COUNTRY)),
                            getValueFromMap(pickupAddress, email), getValueFromMap(pickupAddress, CONTACT_PHONE),
                            getValueFromMap(pickupAddress,ZIP_POST_CODE)
                    );
                    String pickupName = getValueFromMap(pickupAddress, COMPANY_NAME);
                    dictionary.put(ReportConstants.PickupFrom,pickupName);
                    dictionary.put(ReportConstants.PICKUP_FROM_IN_CAPS, pickupName != null ? pickupName.toUpperCase() : "");
                    dictionary.put(ReportConstants.PICKUP_FROM_ADDRESS,pickupAddressList);
                    dictionary.put(ReportConstants.PICKUP_FROM_ADDRESS_IN_CAPS, pickupAddressList.stream().map(String::toUpperCase).toList());

                }
            }
            if(shipment.getPickupDetails() != null) {
                if (shipment.getPickupDetails().getActualPickupOrDelivery() != null) {
                    dictionary.put(ReportConstants.PICKUP_TIME, ConvertToDPWDateFormatWithTime(shipment.getPickupDetails().getActualPickupOrDelivery(), tsDateTimeFormat, true));
                    dictionary.put(ReportConstants.PICKUPTIME_TYPE,  "Actual Pickup");
                } else {
                    if (shipment.getPickupDetails().getEstimatedPickupOrDelivery() != null) {
                        dictionary.put(ReportConstants.PICKUP_TIME, ConvertToDPWDateFormatWithTime(shipment.getPickupDetails().getEstimatedPickupOrDelivery(), tsDateTimeFormat, true));
                    } else {
                        dictionary.put(ReportConstants.PICKUP_TIME, "");
                    }
                    dictionary.put(ReportConstants.PICKUPTIME_TYPE, "Estimated Pickup");
                }

                if(shipment.getPickupDetails().getDestinationDetail() != null) {
                    List<String> cyNameAddress = new ArrayList<>();
                    if(!Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName()))
                        cyNameAddress.add(getValueFromMap(shipment.getPickupDetails().getDestinationDetail().getOrgData(), FULL_NAME));
                    cyNameAddress.addAll(getOrgAddress(shipment.getPickupDetails().getDestinationDetail()));
                    dictionary.put(CY_NAME_ADDRESS, String.join("\r\n", cyNameAddress));
                }

            }
            if(shipment.getReferenceNumbersList() != null)
            {
                List<String> referenceNumberList = shipment.getReferenceNumbersList().stream()
                        .filter(i -> i.getType().equals(DLV)).map(ReferenceNumbersModel::getReferenceNumber).toList();
                if(!referenceNumberList.isEmpty()){
                    dictionary.put(DO_MESSAGE, String.join(",", referenceNumberList));
                }
                referenceNumberList = shipment.getReferenceNumbersList().stream()
                        .filter(i -> i.getType().equals(RAILAGE_PLAYER)).map(ReferenceNumbersModel::getReferenceNumber).toList();
                if(!referenceNumberList.isEmpty()){
                    dictionary.put(RAILAGE_PLAYER, String.join(",", referenceNumberList));
                }
                referenceNumberList = shipment.getReferenceNumbersList().stream()
                        .filter(i -> i.getType().equals(ADDITIONAL_COST_AT)).map(ReferenceNumbersModel::getReferenceNumber).toList();
                if(!referenceNumberList.isEmpty()){
                    dictionary.put(ADDITIONAL_COST_AT, String.join(",", referenceNumberList));
                }
            }
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(ERN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(EXPORTER_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList;
            referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(CEN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(CUSTOMS_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList;
            referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(FRN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(FORWARDER_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
        }
        if(!Strings.isNullOrEmpty(shipment.getCarrierDetails().getShippingLine())){
            CarrierMasterData carrierData = getCarrier(shipment.getCarrierDetails().getShippingLine());
            if(!Objects.isNull(carrierData)) {
                dictionary.put(CARRIER_NAME, carrierData.getItemDescription());
                String iataCode = carrierData.getIataCode();
                dictionary.put(ReportConstants.FLIGHT_IATA_CODE, iataCode);
                dictionary.put(ReportConstants.IATA_CODE, StringUtility.isEmpty(iataCode) ? shipment.getCarrierDetails().getFlightNumber() : iataCode + (StringUtility.isEmpty(shipment.getCarrierDetails().getFlightNumber()) ? "" :(" " + shipment.getCarrierDetails().getFlightNumber())));
                dictionary.put(ReportConstants.SHIPMENT_FLIGHT_NUMBER_WITH_IATACODE, StringUtility.isEmpty(iataCode) ? shipment.getCarrierDetails().getFlightNumber() : iataCode + (StringUtility.isEmpty(shipment.getCarrierDetails().getFlightNumber()) ? "" :(" " + shipment.getCarrierDetails().getFlightNumber())));
            }
        }
        if(!Objects.isNull(pickup) && !Objects.isNull(pickup.getTransporterDetail())){
            dictionary.put(PRE_CARRIAGE_PARTY, pickup.getTransporterDetail().getOrgData() != null &&
                    pickup.getTransporterDetail().getOrgData().containsKey(FULL_NAME1) ?
                    pickup.getTransporterDetail().getOrgData().get(FULL_NAME1) : "");
        }
        if(shipment.getNoOfPacks() != null)
            dictionary.put(ReportConstants.NO_OF_PACKAGES, GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks()), 0, v1TenantSettingsResponse));
        if (!Objects.isNull(shipment.getAdditionalDetails().getWarehouseId()) &&
                wareHouseResponseMap.containsKey(StringUtility.convertToString(shipment.getAdditionalDetails().getWarehouseId()))) {
            dictionary.put(WAREHOUSE_NAME, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getWarehouseId())).getWarehouseDepotName());
            dictionary.put(WAREHOUSE_CODE, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getWarehouseId())).getWarehouseDepotCode());
        }
        if (!Objects.isNull(shipment.getAdditionalDetails().getBondedWarehouseId()) &&
                wareHouseResponseMap.containsKey(StringUtility.convertToString(shipment.getAdditionalDetails().getBondedWarehouseId()))) {
            dictionary.put(BOUNDED_WAREHOUSE_NAME, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getBondedWarehouseId())).getWarehouseDepotName());
            dictionary.put(BOUNDED_WAREHOUSE_CODE, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getBondedWarehouseId())).getWarehouseDepotCode());
        }
        if(shipment.getPickupDetails() != null)
            dictionary.put(PICKUP_SHIPPERS_REF, shipment.getPickupDetails().getShipperRef());
        PartiesModel deliveryTransport = null;
        if(shipment.getDeliveryDetails() != null)
            deliveryTransport = shipment.getDeliveryDetails().getTransporterDetail();
        if (deliveryTransport != null && deliveryTransport.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryTransport.getAddressData();
            populateAddress(addressMap, dictionary, DeliveryTransport);
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put(ReportConstants.DeliveryTransport, address);
        }
        SetContainerCount(shipment, dictionary);
        populateUserFields(UserContext.getUser(), dictionary);
        populateHasContainerFields(shipment, dictionary, v1TenantSettingsResponse);
        if(Objects.equals(shipment.getTransportMode(), AIR))
            dictionary.put(VoyageOrFlightNo, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getFlightNumber() : null);
        else
            dictionary.put(VoyageOrFlightNo, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getVoyage() : null);
        if (!Objects.isNull(dictionary.get(GOODS_VALUE)))
            dictionary.put(GOODS_VALUE, AmountNumberFormatter.Format(new BigDecimal(StringUtility.convertToString(dictionary.get(GOODS_VALUE))), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        if (!Objects.isNull(dictionary.get(INSURANCE_VALUE)))
            dictionary.put(INSURANCE_VALUE, AmountNumberFormatter.Format(new BigDecimal(StringUtility.convertToString(dictionary.get(INSURANCE_VALUE))), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        if (!Objects.isNull(shipment.getNoOfPacks()))
            dictionary.put(ReportConstants.NO_OF_PACKAGES_ALIAS, GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks()), 0, v1TenantSettingsResponse));
        populateIGMInfo(shipment, dictionary);
        dictionary.put(IsDG, false);
        if(Boolean.TRUE.equals(shipment.getContainsHazardous())) {
            dictionary.put(IsDG, true);
            dictionary.put(DGEmergencyContact, getConcatenatedContact(shipment.getAdditionalDetails().getEmergencyContactNumberCode(), shipment.getAdditionalDetails().getEmergencyContactNumber()));
        }
    }

    private String getConcatenatedContact(String code, String number) {
        return Objects.toString(code, "") + " " + Objects.toString(number, "");
    }

    public Map<String, Object> populateHAWBAndSecurityData(List<ShipmentModel> shipmentModelList, List<Awb> awbList, Map<String, Object> dictionary, boolean isSecurity, boolean isShipperAndConsignee, boolean fromConsolidation) {
        List<Object> shipAwbDataList = new ArrayList<>();
        if(shipmentModelList == null)
            shipmentModelList = new ArrayList<>();
        if(awbList == null)
            awbList = new ArrayList<>();
        for (int i=0; i < shipmentModelList.size(); i++) {
            Awb awb = null;
            if(awbList.size() >= i + 1)
                awb = awbList.get(i);
            ShipmentModel shipmentModel = shipmentModelList.get(i);
            Map<String, Object> dict = new HashMap<>();
            if(dictionary == null || fromConsolidation)
                populateShipmentFields(shipmentModel, dict);
            else
                dict = new HashMap<>(dictionary);
            getPackingDetails(shipmentModel, dict);
            dict.put(VOLUME_WEIGHT, ConvertToWeightNumberFormat(shipmentModel.getVolumetricWeight()));
            dict.put(VOLUME_WEIGHT_UNIT, shipmentModel.getVolumetricWeightUnit());
            if(isSecurity)
                dict.put(IS_SECURITY, true);
            else
                dict.put(IS_SECURITY, false);
            populateRaKcData(dict, shipmentModel);
            if(awb != null) {
                if(awb.getAwbSpecialHandlingCodesMappings() != null && !awb.getAwbSpecialHandlingCodesMappings().isEmpty())
                    dict.put(SPH, awb.getAwbSpecialHandlingCodesMappings().stream().map(AwbSpecialHandlingCodesMappingInfo::getShcId).collect(Collectors.toSet()));
                if(awb.getAwbCargoInfo() != null)
                    dict.put(SCI, awb.getAwbCargoInfo().getSci());
            }
            dict.put(WITH_CONSIGNOR, isShipperAndConsignee);
            if(shipmentModel.getDirection().equals(IMP)) {
                try {dict.put(ORIGIN_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getImportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
                try {dict.put(DESTINATION_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getExportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
            }
            else {
                try {dict.put(ORIGIN_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getExportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
                try {dict.put(DESTINATION_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getImportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
            }
            if(shipmentModel.getReferenceNumbersList() != null && !shipmentModel.getReferenceNumbersList().isEmpty()) {
                for (ReferenceNumbersModel referenceNumbersModel: shipmentModel.getReferenceNumbersList()) {
                    if(Objects.equals(referenceNumbersModel.getType(), ReportConstants.MORN) && !dict.containsKey(ReportConstants.MORN))
                        dict.put(ReportConstants.MORN, referenceNumbersModel.getReferenceNumber());
                }
            }
            shipAwbDataList.add(dict);
        }
        if(dictionary == null)
            dictionary = new HashMap<>();
        dictionary.put(SHIPMENT, shipAwbDataList);
        return dictionary;
    }

    public List<String> populateConsigneeData(Map<String, Object> dictionary, PartiesModel shipmentConsignee) {
        List<String> consignee = null;
        if(shipmentConsignee != null)
        {
            Map<String, Object> consigneeAddress = shipmentConsignee.getAddressData();
            if(consigneeAddress != null)
            {
                consignee = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consigneeAddress, COMPANY_NAME), getValueFromMap(consigneeAddress, ADDRESS1),
                        getValueFromMap(consigneeAddress, ADDRESS2),
                        ReportHelper.getCityCountry(getValueFromMap(consigneeAddress, CITY), getValueFromMap(consigneeAddress, COUNTRY)),
                        getValueFromMap(consigneeAddress, "Email"), getValueFromMap(consigneeAddress, CONTACT_PHONE),
                        getValueFromMap(consigneeAddress,ZIP_POST_CODE));
                dictionary.put(ReportConstants.CONSIGNEE_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                dictionary.put(CONSIGNEE_COMPANY_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNEE_CONTACT_PERSON,getValueFromMap(consigneeAddress, CONTACT_PERSON_ALIAS));
                String contactPerson = getValueFromMap(consigneeAddress, CONTACT_PERSON_ALIAS);
                dictionary.put(ReportConstants.CONSIGNEE_PIC, contactPerson == null ? "" : contactPerson.toUpperCase());
                dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, ReportHelper.getOrgAddress(shipmentConsignee));

                try {
                    dictionary.put(ReportConstants.CONSIGNEE_PHONE, consigneeAddress.get("ContactPhone"));
                    dictionary.put(ReportConstants.CONSIGNEE_FULL_NAME, shipmentConsignee.getOrgData().get(FULL_NAME1));
                } catch (Exception ignored) { }
            }
            String consigneeFullName = null;
            if(shipmentConsignee.getOrgData() != null) {
                dictionary.put(ReportConstants.CONSIGNEE_LOCAL_NAME, getValueFromMap(shipmentConsignee.getOrgData(), LOCAL_NAME));
                consigneeFullName = getValueFromMap(shipmentConsignee.getOrgData(), ReportConstants.FULL_NAME);
            }

            if (shipmentConsignee.getIsAddressFreeText() != null && shipmentConsignee.getIsAddressFreeText())
            {
                String rawData = shipmentConsignee.getAddressData() != null && shipmentConsignee.getAddressData().containsKey(PartiesConstants.RAW_DATA)? String.valueOf(shipmentConsignee.getAddressData().get(PartiesConstants.RAW_DATA)): null;
                List<String> consigneeRawAddress = ReportHelper.getAddressList(rawData);
                if(consigneeRawAddress != null && consigneeRawAddress.size() > 0)
                {
                    //Display the consignee name, in case of free text needs to display the first line entered in the address.
                    dictionary.put(ReportConstants.CONSIGNEE_NAME_FREE_TEXT, consigneeRawAddress.get(0).toUpperCase());
                    dictionary.put(CONSIGNEE_FREETEXT, consigneeRawAddress);
                    dictionary.put(CONSIGNEE_FREETEXTInCaps, consigneeRawAddress == null ? null : consigneeRawAddress.stream().map(StringUtility::toUpperCase).toList());
                }
            }
            else
            {
                dictionary.put(ReportConstants.CONSIGNEE_NAME_FREE_TEXT, consigneeFullName == null ? "": consigneeFullName.toUpperCase());
                dictionary.put(CONSIGNEE_FREETEXT, consignee);
            }
        }
        return consignee;
    }

    public ShipmentModel getShipment(Long Id)
    {
        ShipmentDetails shipmentDetails = shipmentDao.findById(Id).get();
        return getShipment(shipmentDetails);
    }

    public ShipmentModel getShipment(ShipmentDetails shipmentDetails) {
        ShipmentModel shipmentModel = modelMapper.map(shipmentDetails, ShipmentModel.class);
        shipmentModel.setVoyage(shipmentDetails.getCarrierDetails().getVoyage());
        try {
            if(shipmentDetails.getContainersList() != null) {
                ContainerSummaryResponse containerSummaryResponse = containerService.calculateContainerSummary(shipmentDetails.getContainersList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType());
                if(containerSummaryResponse != null) {
                    shipmentModel.setSummary(containerSummaryResponse.getSummary());
                }
            }

            if(shipmentDetails.getPackingList() != null) {
                PackSummaryResponse response = packingService.calculatePackSummary(shipmentDetails.getPackingList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType(), new ShipmentMeasurementDetailsDto());
                if(response != null) {
                    shipmentModel.setPackSummary(response.getTotalPacks());
                }
            }

        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return shipmentModel;
    }

    public Map<Long, ShipmentModel> getShipments(List<Long> shipIds) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", shipIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        if(shipmentDetails != null && !shipmentDetails.isEmpty()) {
            Map<Long, ShipmentModel> response = new HashMap<>();
            for (ShipmentDetails shipmentDetails1 : shipmentDetails.getContent()) {
                ShipmentModel shipmentModel = getShipment(shipmentDetails1);
                response.put(shipmentDetails1.getId(), shipmentModel);
            }
            return response;
        }
        return new HashMap<>();
    }

    public ConsolidationModel getFirstConsolidationFromShipmentId(Long shipmentId)
    {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentId);
        if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0) {
            Long id = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getConsolidationId).max(Comparator.naturalOrder()).get();
            return getConsolidation(id);
        }
        return null;
    }

    public TenantModel getTenant()
    {
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().retrieveTenant();
        return modelMapper.map(dependentServiceResponse.getData(), TenantModel.class);
    }

    public ShipmentSettingsDetails getShipmentSettings() {
        ShipmentSettingsDetails tenantSettingsRow = new ShipmentSettingsDetails();
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
        if (shipmentSettingsDetailsList != null && !shipmentSettingsDetailsList.isEmpty()) {
            tenantSettingsRow = shipmentSettingsDetailsList.get(0);
        }
        return tenantSettingsRow;
    }

    public ConsolidationModel getConsolidation(Long Id)
    {
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(Id).get();
        return modelMapper.map(consolidationDetails, ConsolidationModel.class);
    }

    public List<BillingResponse> getBillingData(UUID shipmentGuid) {
        ShipmentGuidRequest request = new ShipmentGuidRequest();
        request.setShipmentGuid(shipmentGuid);
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchBillingList(request);
        return jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class);
    }

    public List<BillChargesResponse> getBillChargesData(UUID billGuid) {
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("BillId"), "=", billGuid));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("IsActive"), "=", 1));
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchBillChargesList(commonV1ListRequest);
        return jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillChargesResponse.class);
    }

    public List<ArObjectResponse> getArObjectData(UUID billGuid) {
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("BillId"), "=", billGuid));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("IsActive"), "=", 1));
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchArObjectList(commonV1ListRequest);
        return jsonHelper.convertValueToList(dependentServiceResponse.getData(), ArObjectResponse.class);
    }

    public ChargeTypesResponse getChargeTypesData(Long chargeTypeId) {
        List<Object> criteria = Arrays.asList(
                Arrays.asList("Id"),
                "=",
                chargeTypeId
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchChargeType(commonV1ListRequest);
        List<ChargeTypesResponse> chargeTypesResponses = jsonHelper.convertValueToList(dependentServiceResponse.getData(), ChargeTypesResponse.class);
        if(chargeTypesResponses != null && chargeTypesResponses.size() > 0)
            return chargeTypesResponses.get(0);
        return null;
    }

    public VesselsResponse getVesselsData(String guid) {
        if(IsStringNullOrEmpty(guid))
            return null;
        List<Object> vesselCriteria = Arrays.asList(
                List.of(Constants.VESSEL_GUID_V1),
                "=",
                guid
        );
        CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(vesselCriteria).build();
        V1DataResponse vesselResponse = v1Service.fetchVesselData(vesselRequest);
        List<VesselsResponse> vesselsResponse = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);
        if(vesselsResponse != null && !vesselsResponse.isEmpty())
            return vesselsResponse.get(0);
        return null;
    }

    public Hbl getHbl(Long Id) {
        List<Hbl> hbls = hblDao.findByShipmentId(Id);
        if(hbls != null && !hbls.isEmpty())
            return hbls.get(0);
        return null;
    }

    public void populateConsolidationFields(ConsolidationModel consolidation, Map<String, Object> dictionary) {
        if (consolidation == null) {
            return;
        }
        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        PartiesModel sendingAgent = consolidation.getSendingAgent();
        PartiesModel receivingAgent = consolidation.getReceivingAgent();
        PartiesModel creditor = consolidation.getCreditor();
        ArrivalDepartureDetailsModel arrivalDetails = consolidation.getArrivalDetails();
        ArrivalDepartureDetailsModel departureDetails = consolidation.getDepartureDetails();
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromConsole(consolidation);
        Map<Integer, Map<String, MasterData>> masterListsMap = fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).toList()).build());
        UnlocationsResponse lastForeignPort = null;
        if (arrivalDetails != null) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                    "=",
                    arrivalDetails.getLastForeignPort()
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            Object unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
            if(unlocationsResponse.size() > 0)
                lastForeignPort = unlocationsResponse.get(0);
        }
        dictionary.put(ReportConstants.CARGO_CLOSING_TIME, consolidation.getCargoClosingTime());
        dictionary.put(ReportConstants.DOCS_CLOSING_TIME, consolidation.getDocsClosingTime());
        dictionary.put(ReportConstants.MASTER_BILL, consolidation.getBol());
        List<String> exportAgentAddress = new ArrayList<>();
        List<String> importAgentAddress = new ArrayList<>();
        List<String> creditorAgentAddress;
        if(sendingAgent != null)
        {
            Map<String, Object> addressData = sendingAgent.getAddressData();
            if(addressData != null)
            {
                exportAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                        StringUtility.convertToString(addressData.get(ADDRESS2)),
                        ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                        null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                        StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                );
                dictionary.put(ReportConstants.SENDING_AGENT_NAME, StringUtility.toUpperCase(StringUtility.convertToString(addressData.get(COMPANY_NAME))));
            }
            Map<String, Object> orgData = sendingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.SENDING_AGENT_LOCAL_NAME, orgData.get(LOCAL_NAME));
                dictionary.put(ReportConstants.SENDING_AGENT_FULL_NAME, orgData.get(FULL_NAME));
            }
        }
        if(receivingAgent != null)
        {
            Map<String, Object> addressData = receivingAgent.getAddressData();
            if(addressData != null)
            {
                importAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                        StringUtility.convertToString(addressData.get(ADDRESS2)),
                        ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                        null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                        StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                );
                dictionary.put(ReportConstants.RECEIVING_AGENT_NAME, StringUtility.toUpperCase(StringUtility.convertToString(addressData.get(COMPANY_NAME))));
            }
            Map<String, Object> orgData = receivingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.RECEIVING_AGENT_LOCAL_NAME, orgData.get(LOCAL_NAME));
                dictionary.put(ReportConstants.RECEIVING_AGENT_FULL_NAME, orgData.get(FULL_NAME));
            }
        }

        List<String> exportAgentFreeTextAddress = new ArrayList<>();
        List<String> importAgentFreeTextAddress = new ArrayList<>();
        List<String> creditorAgentFreeTextAddress;
        if (consolidation.getSendingAgent() != null && consolidation.getIsSendingAgentFreeTextAddress() != null && consolidation.getIsSendingAgentFreeTextAddress()) {
            Map sendingAgentAddressData = consolidation.getSendingAgent().getAddressData();
            if (sendingAgentAddressData != null && sendingAgentAddressData.containsKey(PartiesConstants.RAW_DATA))
                exportAgentFreeTextAddress = ReportHelper.getAddressList(StringUtility.convertToString(sendingAgentAddressData.get(PartiesConstants.RAW_DATA)));
        }
        else {
            exportAgentFreeTextAddress = exportAgentAddress;
        }

        if (consolidation.getReceivingAgent() != null && consolidation.getIsReceivingAgentFreeTextAddress() != null && consolidation.getIsReceivingAgentFreeTextAddress())
        {
            Map importAgentAddressData = consolidation.getReceivingAgent().getAddressData();
                if (importAgentAddressData != null && importAgentAddressData.containsKey(PartiesConstants.RAW_DATA))
                    importAgentFreeTextAddress = ReportHelper.getAddressList(StringUtility.convertToString(importAgentAddressData.get(PartiesConstants.RAW_DATA)));
        }
        else  {
            importAgentFreeTextAddress = importAgentAddress;
        }
        if (Objects.equals(consolidation.getShipmentType(), "EXP"))
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, exportAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, importAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
        }
        else
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, importAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, exportAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
        }

        dictionary.put(ReportConstants.SHIPMENT_TYPE, consolidation.getShipmentType());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, !StringUtility.isEmpty(consolidation.getShipmentType())
                ? consolidation.getShipmentType().toUpperCase().charAt(0) : null);

        dictionary.put(ReportConstants.CONSOL_VESSEL_NAME, consolidation.getCarrierDetails().getVessel());
        dictionary.put(ReportConstants.CONSOL_VOYAGE, consolidation.getCarrierDetails().getVoyage());
        if(creditor != null)
        {
            Map<String, Object> orgData = creditor.getOrgData();
            Map<String, Object> addressData = creditor.getAddressData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.CREDITOR, Arrays.asList(orgData.get(FULL_NAME), creditor.getAddressData() != null ? creditor.getAddressData().get(ADDRESS1) : null));
                dictionary.put(ReportConstants.CREDITOR_LOCAL_NAME, orgData.get(LOCAL_NAME));
                dictionary.put(ReportConstants.CREDITOR_FULL_NAME, orgData.get(FULL_NAME));
            }
            creditorAgentAddress = ReportHelper.getOrgAddress(creditor);
            if (addressData != null)
                dictionary.put(CREDITOR_AGENT_NAME, StringUtility.toUpperCase(StringUtility.convertToString(addressData.get(COMPANY_NAME))));
            if (!Objects.isNull(addressData) && addressData.containsKey(PartiesConstants.RAW_DATA)) {
                creditorAgentFreeTextAddress = ReportHelper.getAddressList(StringUtility.convertToString(addressData.get(PartiesConstants.RAW_DATA)));
            }
            else {
                creditorAgentFreeTextAddress = creditorAgentAddress;
            }
            dictionary.put(ReportConstants.CREDITOR_AGENT_FREETEXT, creditorAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
        }

        dictionary.put(ReportConstants.CONSOL_DATE_OF_DEPARTURE, dictionary.get(ReportConstants.CONSOL_ATD) != null ? dictionary.get(ReportConstants.CONSOL_ATD) : dictionary.get(ReportConstants.CONSOL_ETD));

        if(arrivalDetails != null)
        {
            dictionary.put(ReportConstants.LAST_FOREIGN_PORT, lastForeignPort != null ? lastForeignPort.getName() : null);
            dictionary.put(ReportConstants.LAST_FORGEIN_PORT_COUNTRY, lastForeignPort != null ? lastForeignPort.getCountry() : null);
            dictionary.put(ReportConstants.DEST_PORT_NAME, lastForeignPort != null ? lastForeignPort.getName() : null);
            dictionary.put(ReportConstants.DEST_PORT_COUNTRY, lastForeignPort != null ? lastForeignPort.getCountry() : null);
        }
        dictionary.put(ReportConstants.MSN_NUMBER, consolidation.getMsnNumber());
        dictionary.put(ReportConstants.MRN_NUMBER, consolidation.getMrnNumber());
        dictionary.put(ReportConstants.MASTER_BILL_ISSUE_DATE, consolidation.getMasterBillIssueDate());
        dictionary.put(ReportConstants.CONSOL_REFERENCE_NUMBER, consolidation.getReferenceNumber());
        dictionary.put(ReportConstants.CONSOL_ADDITIONAL_TERMS, consolidation.getAdditionalTerms());
        if(consolidation.getCarrierDetails() != null) {
            dictionary.put(ReportConstants.CONSOL_ETA, consolidation.getCarrierDetails().getEta());
            dictionary.put(ReportConstants.CONSOL_ETD, consolidation.getCarrierDetails().getEtd());
            dictionary.put(ReportConstants.CONSOL_ATA, consolidation.getCarrierDetails().getAta());
            dictionary.put(ReportConstants.CONSOL_ATD, consolidation.getCarrierDetails().getAtd());
            dictionary.put(ReportConstants.CONSOL_FLIGHT_NUMBER, consolidation.getCarrierDetails().getFlightNumber());
            if(StringUtility.isNotEmpty(consolidation.getCarrierDetails().getShippingLine())) {
                CarrierMasterData carriersData = getCarrier(consolidation.getCarrierDetails().getShippingLine());
                if (carriersData != null) {
                    String iataCode = carriersData.getIataCode();
                    dictionary.put(ReportConstants.CONSOL_FLIGHT_NUMBER_WITH_IATACODE,StringUtility.isEmpty(iataCode) ? consolidation.getCarrierDetails().getFlightNumber() : iataCode + " " + consolidation.getCarrierDetails().getFlightNumber());
                    dictionary.put(ReportConstants.IATA_CODE, StringUtility.isEmpty(iataCode) ? consolidation.getCarrierDetails().getFlightNumber() : iataCode + (StringUtility.isEmpty(consolidation.getCarrierDetails().getFlightNumber()) ? "" :(" " + consolidation.getCarrierDetails().getFlightNumber())));
                    dictionary.put(ReportConstants.CARRIER_IATACODE, iataCode);
                    dictionary.put(CARRIER_NAME, StringUtility.toUpperCase(carriersData.getItemDescription()));
                    dictionary.put(CARRIER_CONTACT_PERSON, StringUtility.toUpperCase(carriersData.getCarrierContactPerson()));
                    if (carriersData.getDefaultOrgId() != 0) {
                        List<EntityTransferOrganizations> orgs = masterDataUtils.fetchOrganizations("Id", carriersData.getDefaultOrgId());
                        if (orgs != null && !orgs.isEmpty()) {
                            dictionary.put(ReportConstants.CARRIER_ORG_NAME, StringUtility.toUpperCase(orgs.get(0).getFullName()));
                            dictionary.put(ReportConstants.CARRIER_ORG_PHONE, orgs.get(0).getPhone());
                            dictionary.put(ReportConstants.CARRIER_ORG_FAX, orgs.get(0).getFax());
                        }
                    }
                }
            }
            List<String> unlocoRequests = createUnLocoRequestFromConsolidation(consolidation);
            Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
            UnlocationsResponse pol = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getOriginPort()) : null;
            UnlocationsResponse pod = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getDestinationPort()) : null;
            UnlocationsResponse origin = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getOrigin()) : null;
            UnlocationsResponse destination = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getDestination()) : null;
            if (pol != null && pol.getPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN_PORT_NAME_INCAPS, pol.getPortName().toUpperCase());
            }
            if (pod != null && pod.getPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION_PORT_NAME_INCAPS, pod.getPortName().toUpperCase());
            }
            if(origin != null && origin.getPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN, origin.getPortName().toUpperCase());
            }
            if(destination != null && destination.getPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION, destination.getPortName().toUpperCase());
            }
            if (pol != null && pol.getAirPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN_PORT_NAME_INCAPS_AIR, pol.getAirPortName().toUpperCase());
            }
            if (pod != null && pod.getAirPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION_PORT_NAME_INCAPS_AIR, pod.getAirPortName().toUpperCase());
            }
            if(origin != null && origin.getAirPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN_AIR, origin.getAirPortName().toUpperCase());
            }
            if(destination != null && destination.getAirPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION_AIR, destination.getAirPortName().toUpperCase());
            }
        }

        PartiesModel notifyParty = null;
        List<PartiesModel> consolidationAddresses = consolidation.getConsolidationAddresses();
        for (PartiesModel consolidationAddress : consolidationAddresses) {
            if(Objects.equals(consolidationAddress.getType(), "Notify Party 1"))
            {
                notifyParty = consolidationAddress;
            }
        }
        if(notifyParty != null){
            Map<String, Object> addressData = notifyParty.getAddressData();
            List<String> consolNotifyPartyAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                    StringUtility.convertToString(addressData.get(ADDRESS2)),
                    ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                    null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                    StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
            );
            dictionary.put(ReportConstants.CONSOL_NOTIFY_ADDRESS, consolNotifyPartyAddress);
        }
        dictionary.put(CONSOLE_DELIVERY_MODE_IN_CAPS, StringUtility.toUpperCase(consolidation.getDeliveryMode()));
        dictionary.put(CONSOLE_MARKS_N_NUMBERS_IN_CAPS, StringUtility.toUpperCase(consolidation.getMrnNumber()));
        MasterData masterdata = MasterData.builder().build();
        if (masterListsMap.containsKey(MasterDataType.PAYMENT.getId()) && masterListsMap.get(MasterDataType.PAYMENT.getId()).containsKey(consolidation.getPayment()))
            masterdata = masterListsMap.get(MasterDataType.PAYMENT.getId()).get(consolidation.getPayment());
        dictionary.put(CONSOLE_PAYMENT_TERMS_IN_CAPS, StringUtility.toUpperCase(masterdata != null ? masterdata.getItemDescription() : consolidation.getPayment()));
        dictionary.put(CONSOLE_PAYMENT_TERMS, StringUtility.toUpperCase(consolidation.getPayment()));

        if (consolidation.getAllocations() != null) {
            dictionary.put(CONSOLIDATED_WEIGHT, addCommas(consolidation.getAllocations().getWeight()));
            dictionary.put(CONSOLE_WEIGHT_CODE, StringUtility.toUpperCase(consolidation.getAllocations().getWeightUnit()));
            dictionary.put(CONSOLIDATED_VOLUME, addCommas(consolidation.getAllocations().getVolume()));
            dictionary.put(CONSOLE_VOLUME_CODE, StringUtility.toUpperCase(consolidation.getAllocations().getVolumeUnit()));
            if (masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getId()) && masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).containsKey(consolidation.getAllocations().getWeightUnit()))
                dictionary.put(CONSOLE_WEIGHT_DESCRIPTION, StringUtility.toUpperCase(masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).get(consolidation.getAllocations().getWeightUnit()).getItemDescription()));
            if (masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getId()) && masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).containsKey(consolidation.getAllocations().getVolumeUnit()))
                dictionary.put(CONSOLE_VOLUME_DESCRIPTION, StringUtility.toUpperCase(masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).get(consolidation.getAllocations().getVolumeUnit()).getItemDescription()));
        }
        dictionary.put(SI_CUT_OFF_TIME, consolidation.getShipInstructionCutoff());
        if(departureDetails != null) {
            if(departureDetails.getCTOId() != null){
                dictionary.put(CTO_FULL_NAME, getValueFromMap(departureDetails.getCTOId().getOrgData(), FULL_NAME));
                dictionary.put(TERMINAL, dictionary.get(CTO_FULL_NAME));
            }
            if(departureDetails.getContainerYardId() != null) {
                List<String> cyNameAddress = new ArrayList<>();
                if(!Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName()))
                    cyNameAddress.add(getValueFromMap(departureDetails.getContainerYardId().getOrgData(), FULL_NAME));
                cyNameAddress.addAll(getOrgAddress(departureDetails.getContainerYardId()));
                dictionary.put(CY_NAME_ADDRESS, String.join("\r\n", cyNameAddress));
            }
        }
        populateUserFields(UserContext.getUser(), dictionary);
        dictionary.put(IsDG, false);
        if(Boolean.TRUE.equals(consolidation.getHazardous())) {
            dictionary.put(IsDG, true);
            dictionary.put(DGEmergencyContact, getConcatenatedContact(consolidation.getEmergencyContactNumberCode(), consolidation.getEmergencyContactNumber()));
        }
    }

    public void populateBlFields(Hbl hbl, Map<String, Object> dictionary)
    {
        if (hbl == null) return;
        List<String> notify = new ArrayList<>();
        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(hbl.getHblNotifyParty() != null && hbl.getHblNotifyParty().size() > 0) {
            HblPartyDto hblNotify = hbl.getHblNotifyParty().get(0);
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName()))
                notify = ReportHelper.getOrgAddress(null, hblNotify.getAddress(), null, null, hblNotify.getEmail(), null);
            else
                notify = ReportHelper.getOrgAddress(hblNotify.getName(), hblNotify.getAddress(), null, null, hblNotify.getEmail(), null);
        }
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromHbl(hbl);
        Map<Integer, Map<String, MasterData>> masterListsMap = fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).toList()).build());
        dictionary.put(ReportConstants.BL_NOTIFY_PARTY, notify);
            dictionary.put(ReportConstants.BL_NOTIFY_PARTY_CAPS, notify.stream().map(String::toUpperCase).toList());
        HblDataDto hblDataDto = hbl.getHblData();
        List<String> consignor;
        List<String> consignee;
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName())) {
            consignor = ReportHelper.getOrgAddress(null, hblDataDto != null ? hblDataDto.getConsignorAddress() : null, null, null, null, null);
            consignee = ReportHelper.getOrgAddress(null, hblDataDto != null ? hblDataDto.getConsigneeAddress() : null, null, null, null, null);
        } else {
            consignor = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsignorName() : null, hblDataDto != null ? hblDataDto.getConsignorAddress() : null, null, null, null, null);
            consignee = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsigneeName() : null, hblDataDto != null ? hblDataDto.getConsigneeAddress() : null, null, null, null, null);
        }
        if(consignor != null && consignor.size() > 0) {
            dictionary.put(ReportConstants.CONSIGNER, consignor);
            dictionary.put(ReportConstants.CONSIGNER_NAME_FREETEXT_INCAPS, StringUtility.toUpperCase(hblDataDto.getConsignorName()));
        }
        if(consignee != null && consignee.size() > 0) {
            dictionary.put(ReportConstants.CONSIGNEE, consignee);
            dictionary.put(ReportConstants.CONSIGNEE_NAME_FREE_TEXT, consignee.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(CONSIGNEE_NAME_FREETEST_INCAPS, StringUtility.toUpperCase(hblDataDto.getConsigneeName()));
        }
        if(notify != null && notify.size() > 0) {
            dictionary.put(ReportConstants.NOTIFY_PARTY_NAME_FREETEXT_INCAPS, notify.stream().map(StringUtility::toUpperCase).toList());
        }

        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.PRINT_DATE, ConvertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat));

        if(StringUtility.isNotEmpty(hblDataDto.getCargoDescription())) {
            dictionary.put(ReportConstants.DO_MESSAGE, hblDataDto.getCargoDescription());
        }
        dictionary.put(ReportConstants.CONSIGNER_CAPS, consignor.stream().map(String::toUpperCase).toList());
        dictionary.put(ReportConstants.CONSIGNEE_CAPS, consignee.stream().map(String::toUpperCase).toList());
        if (!StringUtility.isEmpty(hblDataDto.getHouseBill()))
            dictionary.put(ReportConstants.HOUSE_BILL, hblDataDto.getHouseBill());
        dictionary.put(ReportConstants.BL_VESSEL_NAME, hblDataDto.getVesselName());
        dictionary.put(ReportConstants.BL_VOYAGE, hblDataDto.getVoyage());
        dictionary.put(ReportConstants.PORT_OF_LOADING, hblDataDto.getPortOfLoad());
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, hblDataDto.getPortOfDischarge());
        dictionary.put(ReportConstants.MARKS_N_NUMS, hblDataDto.getMarksAndNumbers());
        dictionary.put(ReportConstants.MARKS_N_NUMS_CAPS, hblDataDto.getMarksAndNumbers() != null ? hblDataDto.getMarksAndNumbers().toUpperCase() : null);
        dictionary.put(ReportConstants.PACKS, GetDPWWeightVolumeFormat(BigDecimal.valueOf(hblDataDto.getPackageCount()), 0, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.PACKS_UNIT, Constants.MPK.equals(hblDataDto.getPackageType()) ? Constants.PACKAGES : hblDataDto.getPackageType());
        if(StringUtility.isNotEmpty(hblDataDto.getCargoDescription())) {
            dictionary.put(ReportConstants.DESCRIPTION, hblDataDto.getCargoDescription());
            dictionary.put(ReportConstants.DESCRIPTION_CAPS, hblDataDto.getCargoDescription() != null ? hblDataDto.getCargoDescription().toUpperCase() : null);
        }
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, hblDataDto.getPlaceOfDelivery());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT, ConvertToWeightNumberFormat(hblDataDto.getCargoNetWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT_UNIT, hblDataDto.getCargoNetWeightUnit());
        dictionary.put(ReportConstants.FINAL_DESTINATION, hblDataDto.getFinalDestination());
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME, ConvertToVolumeNumberFormat(hblDataDto.getCargoGrossVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME_UNIT, hblDataDto.getCargoGrossVolumeUnit());
        if (masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getId()) && masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).containsKey(hblDataDto.getCargoGrossVolumeUnit()))
            dictionary.put(CARGO_GROSS_VOLUME_UNIT_DESCRIPTION, StringUtility.toUpperCase(StringUtility.convertToString(masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).get(hblDataDto.getCargoGrossVolumeUnit()))));
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT, ConvertToWeightNumberFormat(hblDataDto.getCargoGrossWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT_UNIT, hblDataDto.getCargoGrossWeightUnit());
        if (masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getId()) && masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).containsKey(hblDataDto.getCargoGrossVolumeUnit()))
            dictionary.put(CARGO_GROSS_WEIGHT_UNIT_DESCRIPTION, StringUtility.toUpperCase(StringUtility.convertToString(masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).get(hblDataDto.getCargoGrossWeightUnit()))));
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_COUNT, hblDataDto.getPackageCount());
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_TYPE, hblDataDto.getPackageType());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY, hblDataDto.getQuantity());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY_CODE, hblDataDto.getQuantityCode());
        dictionary.put(ReportConstants.PLACE_OF_RECEIPT, hblDataDto.getPlaceOfReceipt());
        if(StringUtility.isNotEmpty(hblDataDto.getPlaceOfReceipt())) {
            dictionary.put(ReportConstants.PLACE_OF_RECIEPT_IN_CAPS, hblDataDto.getPlaceOfReceipt().toUpperCase());
        }
        dictionary.put(ReportConstants.BL_COMMENTS, StringUtility.toUpperCase(hblDataDto.getBlComments()));
        dictionary.put(ReportConstants.BLComments, StringUtility.toUpperCase(hblDataDto.getBlComments()));
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT, hblDataDto.getDeliveryAgent());
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT_ADDRESS, hblDataDto.getDeliveryAgentAddress());
        dictionary.put(ReportConstants.BL_CARGO_TERMS_DESCRIPTION, StringUtility.toUpperCase(hblDataDto.getCargoTermsDescription()));
        dictionary.put(ReportConstants.BL_REMARKS_DESCRIPTION, StringUtility.toUpperCase(hblDataDto.getBlRemarksDescription()));
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME_WITH_COMMA, ConvertToVolumeNumberFormat(hblDataDto.getCargoGrossVolume(), v1TenantSettingsResponse));
    }

    public void populateUserFields(UsersDto user, Map<String, Object> dictionary) {
        if (user == null) {
            return;
        }
        dictionary.put(USER_DISPLAY_NAME, user.DisplayName);
        dictionary.put(ReportConstants.USER_FULLNAME, user.DisplayName);
        dictionary.put(ReportConstants.TENANT_NAME, user.TenantDisplayName);
        dictionary.put(ReportConstants.USER_NAME, user.Username);
        dictionary.put(PRINT_USER, user.Username.toUpperCase());
        dictionary.put(ReportConstants.USER_EMAIL, user.Email);
        dictionary.put(USER_PHONE_NUMBER, user.Phone);
        dictionary.put(ReportConstants.TENANT_CURRENCY, user.CompanyCurrency);
    }

    public MasterData getMasterListData(MasterDataType type, String ItemValue) {
        return masterDataUtils.getMasterListData(type, ItemValue);
    }
    public CarrierMasterData getCarrier(String carrier) {
        if(StringUtility.isEmpty(carrier)) return null;
        List<Object> carrierCriteria = Arrays.asList(
                List.of("ItemValue"),
                "=",
                carrier
        );
        CommonV1ListRequest carrierRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(carrierCriteria).build();
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(carrierRequest);
        carrierListObject.setIsList(true);
        Object carrierResponse = masterDataFactory.getMasterDataService().fetchCarrierMasterData(carrierListObject).getData();
        List<CarrierMasterData> carrierMasterData = jsonHelper.convertValueToList(carrierResponse, CarrierMasterData.class);
        if(carrierMasterData == null || carrierMasterData.isEmpty())
            return null;
        return carrierMasterData.get(0);
    }

    public List<ContainerCountByCode> getCountByContainerTypeCode(List<ShipmentContainers> commonContainers) {
        Map<String, Long> containerTypeCountMap = new HashMap<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        if (commonContainers != null) {
            for(ShipmentContainers container : commonContainers) {
                if (container.ContainerTypeCode != null) {
                    Long containerCount = container.getContainerCount();
                    if (!containerTypeCountMap.containsKey(container.ContainerTypeCode)) {
                        containerTypeCountMap.put(container.ContainerTypeCode, containerCount);
                        continue;
                    }
                    Long containers = containerTypeCountMap.get(container.ContainerTypeCode);
                    containers += containerCount;
                    containerTypeCountMap.put(container.ContainerTypeCode, containers);
                }
            }
        }

        List<ContainerCountByCode> containerCountByCode = new ArrayList<>();
        for (var entry : containerTypeCountMap.entrySet()) {
            ContainerCountByCode countByCode = new ContainerCountByCode();
            countByCode.ContainerTypeCode = entry.getKey();
            countByCode.ContainerCount = GetDPWWeightVolumeFormat(BigDecimal.valueOf(entry.getValue()), 0, v1TenantSettingsResponse);
            containerCountByCode.add(countByCode);
        }
        return containerCountByCode;
    }

    public List<ContainerCountByCode> getCountByCommonContainerTypeCode(List<ContainerModel> commonContainers) {
        Map<String, Long> containerTypeCountMap = new HashMap<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        if (commonContainers != null) {
            for (var container : commonContainers) {
                if (container.getContainerCode() != null) {
                    Long containerCount = container.getContainerCount();
                    if (!containerTypeCountMap.containsKey(container.getContainerCode())) {
                        containerTypeCountMap.put(container.getContainerCode(), containerCount);
                        continue;
                    }
                    Long containers = containerTypeCountMap.get(container.getContainerCode());
                    containers += containerCount;
                    containerTypeCountMap.put(container.getContainerCode(), containers);
                }
            }
        }

        List<ContainerCountByCode> containerCountByCode = new ArrayList<>();
        for (var entry : containerTypeCountMap.entrySet()) {
            ContainerCountByCode countByCode = new ContainerCountByCode();
            countByCode.ContainerTypeCode = entry.getKey();
            countByCode.ContainerCount = GetDPWWeightVolumeFormat(BigDecimal.valueOf(entry.getValue()), 0, v1TenantSettingsResponse);
            containerCountByCode.add(countByCode);
        }
        return containerCountByCode;
    }

    public Awb getHawb(Long Id) {
        List<Awb> awb = awbRepository.findByShipmentId(Id);
        if (awb != null && !awb.isEmpty())
            return awb.get(0);
        return null;
    }

    public Awb getMawb(Long Id) {
        List<Awb> awb = awbRepository.findByConsolidationId(Id);
        if(awb != null && !awb.isEmpty()) {
            awbService.getMawnLinkPacks(awb.get(0));
            return awb.get(0);
        }
        return null;
    }

    public static List<String> getFormattedDetails(String name, String address)
    {
        List<String> details = new ArrayList<>();
        details.add(name);
        if(StringUtility.isNotEmpty(address)) {
            String[] addressList = address.split("\r\n");
            addressList = Arrays.stream(addressList)
                    .filter(Objects::nonNull)
                    .map(String::trim)
                    .filter(Predicate.isEqual("").negate())
                    .toArray(String[]::new);
            details.addAll(Arrays.asList(addressList));
        }
        return details;
    }

    public static String addCommas(BigDecimal amount)
    {
        if (amount == null) return null;
        DecimalFormat decimalFormat = new DecimalFormat("#,##0.00");
        return decimalFormat.format(amount);
    }

    public static String addCommas(Integer amount)
    {
        if (amount == null) return null;
        DecimalFormat decimalFormat = new DecimalFormat("#,##0.00");
        return decimalFormat.format(amount);
    }

    public static String addCommas(String amount)
    {
        if (amount == null)
        {
            return null;
        }
        try{
            return addCommas(new BigDecimal(amount));
        }
        catch (Exception ex)
        {
            return amount;
        }
    }

    public static String appendZero(String value, int length){
        int size = value.length();
        for(int i=0; i<length-size; i++){
            value = "0" + value;
        }
        return value;
    }

    public void JsonDateFormat(Map<String, Object> dictionary) {
        if (dictionary != null) {
            Map<String, Object> dictionaryCopy = new LinkedHashMap<>(dictionary);
            for (Map.Entry<String, Object> entry : dictionaryCopy.entrySet()) {
                Object value = entry.getValue();
                if (value != null && value instanceof LocalDateTime) {
                    LocalDateTime val = (LocalDateTime) value;
                    dictionary.put(entry.getKey(), ConvertToDPWDateFormat(val));
                }
            }
        }
    }

    public static String twoDecimalPlacesFormatDecimal(BigDecimal value)
    {
        if(value == null)
        {
            return "0.00";
        }
        NumberFormat customFormat = NumberFormat.getNumberInstance(Locale.US);
        DecimalFormat customDecimalFormat = (DecimalFormat) customFormat;
        customDecimalFormat.setMaximumFractionDigits(2);
        customDecimalFormat.setMinimumFractionDigits(2);
        return customDecimalFormat.format(value);
    }

    public DateTimeFormatter GetDPWDateFormatOrDefault()
    {
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        if(!CommonUtils.IsStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
            return DateTimeFormatter.ofPattern(v1TenantSettingsResponse.getDPWDateFormat());
        return DateTimeFormatter.ofPattern("MM/dd/yyyy");
    }

    public static DateTimeFormatter GetDPWDateFormatWithTime(String tsDatetimeFormat)
    {
        if(StringUtility.isNotEmpty(tsDatetimeFormat)) {
            return DateTimeFormatter.ofPattern(tsDatetimeFormat+" HH:mm:ss");
        }
        return DateTimeFormatter.ofPattern("MM/dd/yyyy HH:mm:ss");
    }

    public static String GetDPWDateFormatOrDefaultString()
    {
        return "MM/dd/yyyy";
    }

    public String ConvertToDPWDateFormat(LocalDateTime date) {
        return ConvertToDPWDateFormat(date, null);
    }
    public String ConvertToDPWDateFormat(LocalDateTime date, String tsDatetimeFormat)
    {
        String strDate = "";
        if (date != null)
        {
            if(!IsStringNullOrEmpty(tsDatetimeFormat))
                strDate = date.format(DateTimeFormatter.ofPattern(tsDatetimeFormat));
            else
                strDate = date.format(GetDPWDateFormatOrDefault());
        }
        return strDate;
    }

    public static String ConvertToDPWDateFormatWithTime(LocalDateTime date, String tsDatetimeFormat, boolean isTimeZone)
    {
        String strDate = "";
        if (date != null)
        {
            if(isTimeZone) {
                strDate = LocalTimeZoneHelper.getDateTime(date).format(GetDPWDateFormatWithTime(tsDatetimeFormat));
            } else {
                strDate = date.format(GetDPWDateFormatWithTime(tsDatetimeFormat));
            }
        }
        return strDate;
    }

    public String ConvertToDPWDateFormat(LocalDateTime date, String tsDatetimeFormat, boolean isTimeZone)
    {
        String strDate = "";
        LocalDateTime formatedDate;
        if (date != null)
        {
            formatedDate = date;
            if(isTimeZone) {
                formatedDate =  LocalTimeZoneHelper.getDateTime(date);
            }
            if(!IsStringNullOrEmpty(tsDatetimeFormat))
                strDate = formatedDate.format(DateTimeFormatter.ofPattern(tsDatetimeFormat));
            else
                strDate = formatedDate.format(GetDPWDateFormatOrDefault());
        }
        return strDate;
    }

    public String ConvertToWeightNumberFormat(BigDecimal weight) {
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        return ConvertToWeightNumberFormat(weight, v1TenantSettingsResponse);
    }

    public static String ConvertToWeightNumberFormat(Object weight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(weight != null && !CommonUtils.IsStringNullOrEmpty(weight.toString())) {
            return ConvertToWeightNumberFormat(new BigDecimal(weight.toString()), v1TenantSettingsResponse);
        }
        return null;
    }
    public static String ConvertToWeightNumberFormat(BigDecimal weight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(weight != null) {
            int numberDecimalDigits = 0;
            if(v1TenantSettingsResponse.getWeightDecimalPlace() != null)
                numberDecimalDigits = v1TenantSettingsResponse.getWeightDecimalPlace();
            return GetDPWWeightVolumeFormat(weight, numberDecimalDigits, v1TenantSettingsResponse);
        }
        return null;
    }

    public String ConvertToVolumeNumberFormat(BigDecimal volume) {
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        return ConvertToVolumeNumberFormat(volume, v1TenantSettingsResponse);
    }
    public static String ConvertToVolumeNumberFormat(Object volume, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(volume != null && !CommonUtils.IsStringNullOrEmpty(volume.toString())) {
            return ConvertToVolumeNumberFormat(new BigDecimal(volume.toString()), v1TenantSettingsResponse);
        }
        return null;
    }
    public static String ConvertToVolumeNumberFormat(BigDecimal volume, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(volume != null) {
            int numberDecimalDigits = 0;
            if(v1TenantSettingsResponse.getVolumeDecimalPlace() != null)
                numberDecimalDigits = v1TenantSettingsResponse.getVolumeDecimalPlace();
            return GetDPWWeightVolumeFormat(volume, numberDecimalDigits, v1TenantSettingsResponse);
        }
        return null;
    }

    public static String ConvertToVolumetricWeightFormat(BigDecimal weight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(weight != null) {
            int numberDecimalDigits = 2;
            if(v1TenantSettingsResponse.getDecimalValueForVolumetricWeight() != null)
                numberDecimalDigits = v1TenantSettingsResponse.getDecimalValueForVolumetricWeight();
            return GetDPWWeightVolumeFormat(weight, numberDecimalDigits, v1TenantSettingsResponse);
        }
        return null;
    }

    public static String addCommasWithPrecision(BigDecimal number, int decimalPlaces) {
        if (number != null) {
            try {
                BigDecimal roundedNumber = number.setScale(decimalPlaces, RoundingMode.HALF_UP);
                DecimalFormat decimalFormat = new DecimalFormat();
                decimalFormat.setMaximumFractionDigits(decimalPlaces);
                return decimalFormat.format(roundedNumber);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return String.valueOf(number);
    }

    public static String DisplayFormat(BigDecimal value, int numberDecimalDigits, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(value != null && v1TenantSettingsResponse != null) {
            if(v1TenantSettingsResponse.getCurrencyDigitGrouping() != null) {
                char customThousandsSeparator = ',';
                char customDecimalSeparator = '.';
                if(v1TenantSettingsResponse.getCurrencyGroupingNumber() != null && v1TenantSettingsResponse.getCurrencyGroupingNumber() == GroupingNumber.DotAndComma.getValue()) {
                    customThousandsSeparator = '.';
                    customDecimalSeparator = ',';
                }
                return formatValue(value, customDecimalSeparator, customThousandsSeparator, numberDecimalDigits, v1TenantSettingsResponse.getCurrencyDigitGrouping());
            }
            else {
                return addCommasWithPrecision(value, numberDecimalDigits);
            }
        }
        if(value != null)
            return value.toString();
        return null;
    }

    public static String GetDPWWeightVolumeFormat(BigDecimal value, int numberDecimalDigits, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(value != null && v1TenantSettingsResponse != null) {
            if(v1TenantSettingsResponse.getWVDigitGrouping() != null) {
                char customThousandsSeparator = ',';
                char customDecimalSeparator = '.';
                if(v1TenantSettingsResponse.getWVGroupingNumber() != null && v1TenantSettingsResponse.getWVGroupingNumber() == GroupingNumber.DotAndComma.getValue()) {
                    customThousandsSeparator = '.';
                    customDecimalSeparator = ',';
                }
                return formatValue(value, customDecimalSeparator, customThousandsSeparator, numberDecimalDigits, v1TenantSettingsResponse.getWVDigitGrouping());
            }
            else {
                return addCommasWithPrecision(value, numberDecimalDigits);
            }
        }
        if(value != null)
            return value.toString();
        return null;
    }

    public static String formatValue(BigDecimal value, char customDecimalSeparator, char customThousandsSeparator, int numberDecimalDigits, Integer digitGrouping) {
        if(value == null)
            return null;
        int dynamicGroupSizes = (digitGrouping == DigitGrouping.THREE.getValue()) ? 3 : 2;

        NumberFormat customFormat = NumberFormat.getNumberInstance(Locale.US);
        DecimalFormat customDecimalFormat = (DecimalFormat) customFormat;

        customDecimalFormat.setDecimalSeparatorAlwaysShown(true);
        customDecimalFormat.setMaximumFractionDigits(numberDecimalDigits);
        customDecimalFormat.setMinimumFractionDigits(numberDecimalDigits);

        DecimalFormatSymbols symbols = customDecimalFormat.getDecimalFormatSymbols();
        symbols.setDecimalSeparator(customDecimalSeparator);
        symbols.setGroupingSeparator(customThousandsSeparator);
        customDecimalFormat.setDecimalFormatSymbols(symbols);

        customDecimalFormat.setGroupingUsed(true);
        customDecimalFormat.setGroupingSize(3);

        String result = customDecimalFormat.format(value);
        if(numberDecimalDigits == 0)
            result = result.substring(0, result.length() - 1);
        if(dynamicGroupSizes == 3)
            return result;
        StringBuilder formattedResult = new StringBuilder();
        int count = 0;
        boolean threeDigitSeparatorAdded = false;
        boolean decimalPassed = numberDecimalDigits == 0;
        for (int i = result.length() - 1; i >= 0; i--) {
            char c = result.charAt(i);
            if(c != customThousandsSeparator && c != '-') {
                if (c == customDecimalSeparator) {
                    decimalPassed = true;
                } else if (decimalPassed && count == 2 && threeDigitSeparatorAdded) {
                    formattedResult.insert(0, customThousandsSeparator);
                    count = 1;
                } else if(decimalPassed && count == 3 && !threeDigitSeparatorAdded) {
                    threeDigitSeparatorAdded = true;
                    formattedResult.insert(0, customThousandsSeparator);
                    count = 1;
                }
                else if(decimalPassed) {
                    count++;
                }
                formattedResult.insert(0, c);
            }
            if(c == '-')
                formattedResult.insert(0, c);
        }
        return formattedResult.toString();
    }

    public static BigDecimal getRoundedBigDecimal(BigDecimal value, int scale, RoundingMode roundingMode) {
        if(value == null) {
            return null;
        }

        return value.setScale(scale, roundingMode);
    }

    public String getValueFromMap(Map<String, Object> dataMap, String key) {
        if (Objects.isNull(dataMap))
            return null;

        Object value = dataMap.get(key);
        if(value == null || ! (value instanceof String)) {
            return null;
        }
        return value.toString();
    }

    public String getPortDetails(String UNLocCode) {
        UnlocationsResponse unlocationsResponse = getUNLocRow(UNLocCode);
        if(unlocationsResponse != null) {
            return combineStringsWithComma(unlocationsResponse.getName(), unlocationsResponse.getCountry());
        }
        return "";
    }

    public UnlocationsResponse getUNLocRow(String UNLocCode) {
        return masterDataUtils.getUNLocRow(UNLocCode);
    }

    public CommodityResponse getCommodity(String commodityCode) {
        if(commodityCode == null || commodityCode.isEmpty())
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList("Code"),
                "=",
                commodityCode
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        Object commodity = masterDataFactory.getMasterDataService().fetchCommodityData(commonV1ListRequest).getData();
        List<CommodityResponse> commodityResponses = jsonHelper.convertValueToList(commodity, CommodityResponse.class);
        if(commodityResponses.size() > 0)
            return commodityResponses.get(0);
        return null;
    }


    public List<PackingModel> GetAllShipmentsPacks(List<ShipmentModel> shipmentDetails){
        List<PackingModel> packingList = new ArrayList<>();
        if(shipmentDetails != null) {
            for(var shipment : shipmentDetails) {
                packingList.addAll(shipment.getPackingList());
            }
        }
        return packingList;
    }
    public Pair<BigDecimal, String> GetTotalWeight(List<PackingModel> packingList) {
        Pair<BigDecimal, String> res = Pair.of(BigDecimal.ZERO, null);
        if(packingList != null ) {
            String weightUnit = null;
            BigDecimal totalWeight = BigDecimal.ZERO;

            for(var packing : packingList) {
                if(packing.getWeight() != null && !IsStringNullOrEmpty(packing.getWeightUnit())) {
                    if(weightUnit == null) {
                        weightUnit = packing.getWeightUnit();
                    }
                    if(!Objects.equals(packing.getWeightUnit(), weightUnit))
                        return res;
                    totalWeight = totalWeight.add(packing.getWeight());
                }
            }
            return Pair.of(totalWeight, weightUnit);
        }
        return res;
    }

    public Pair<BigDecimal, String> GetTotalVolume(List<PackingModel> packingList) {
        Pair<BigDecimal, String> res = Pair.of(BigDecimal.ZERO, null);
        if(packingList != null ) {
            String volumeUnit = null;
            BigDecimal totalVolume = BigDecimal.ZERO;

            for(var packing : packingList) {
                if(packing.getVolume() != null && !IsStringNullOrEmpty(packing.getVolumeUnit())) {
                    if(volumeUnit == null) {
                        volumeUnit = packing.getVolumeUnit();
                    }
                    if(!Objects.equals(packing.getVolumeUnit(), volumeUnit))
                        return res;
                    totalVolume = totalVolume.add(packing.getVolume());
                }
            }
            return Pair.of(totalVolume, volumeUnit);
        }
        return res;
    }

    public List<ShipmentAndContainerResponse> getShipmentAndContainerResponse(List<ShipmentModel> shipments) {
        List<ShipmentAndContainerResponse> shipmentContainers = new ArrayList<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        if(shipments == null)
            return shipmentContainers;

        for(var shipment : shipments) {
            ShipmentAndContainerResponse shipmentContainer = new ShipmentAndContainerResponse();

            shipmentContainer.hsnNumber = shipment.getAdditionalDetails().getHsnNumber() != null ?
                    shipment.getAdditionalDetails().getHsnNumber().toString(): null;
            shipmentContainer.houseBill = shipment.getHouseBill();
            shipmentContainer.masterBill = shipment.getMasterBill();
            shipmentContainer.description = StringUtility.toUpperCase(shipment.getGoodsDescription());
            shipmentContainer.weight = ConvertToWeightNumberFormat(StringUtility.convertToString(shipment.getWeight()), v1TenantSettingsResponse);
            shipmentContainer.volume = ConvertToVolumeNumberFormat(StringUtility.convertToString(shipment.getVolume()), v1TenantSettingsResponse);
            shipmentContainer.packs = GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks() != null ? shipment.getNoOfPacks() : 0), 0, v1TenantSettingsResponse);
            shipmentContainer.packsUnit = StringUtility.convertToString(shipment.getPacksUnit());
            shipmentContainer.weightUnit = StringUtility.convertToString(shipment.getWeightUnit());
            shipmentContainer.volumeUnit = StringUtility.convertToString(shipment.getVolumeUnit());
            shipmentContainer.volumeUnitDescription = StringUtility.toUpperCase(shipment.getVolumeUnit());
            shipmentContainer.weightUnitDescription = StringUtility.toUpperCase(shipment.getWeightUnit());
            shipmentContainer.packsUnitDescription = StringUtility.toUpperCase(shipment.getPacksUnit());
            shipmentContainer.marksnNumbers = StringUtility.toUpperCase(shipment.getMarksNum());
            shipmentContainer.freightOverseasCurrency = shipment.getFreightOverseasCurrency();

            PartiesModel consigner = shipment.getConsigner();
            if(consigner != null && consigner.getAddressData() != null) {

                shipmentContainer.consignerCompanyName = consigner.getAddressData().get(COMPANY_NAME) != null ? StringUtility.toUpperCase(consigner.getAddressData().get(COMPANY_NAME).toString()) : null;
                shipmentContainer.consignerAddress1 = consigner.getAddressData().get(ADDRESS1) != null ? consigner.getAddressData().get(ADDRESS1).toString() : null;
                shipmentContainer.consignerAddress2 = consigner.getAddressData().get(ADDRESS2) != null ? consigner.getAddressData().get(ADDRESS2).toString() : null;
                shipmentContainer.consignerCountry = consigner.getAddressData().get(COUNTRY) != null ? consigner.getAddressData().get(COUNTRY).toString() : null;
                shipmentContainer.consignerZip = consigner.getAddressData().get(ZIP_POST_CODE) != null ? consigner.getAddressData().get(ZIP_POST_CODE).toString() : null;
                if (consigner.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
                    List<String> rawData = getAddressList(StringUtility.convertToString(consigner.getAddressData().containsKey(PartiesConstants.RAW_DATA)));
                    shipmentContainer.setConsignerAddressFreeText(rawData.stream().map(StringUtility::toUpperCase).toList());
                }
            }

            PartiesModel consignee = shipment.getConsignee();
            if(consignee != null && consignee.getAddressData() != null) {
                shipmentContainer.consigneeCompanyName = consignee.getAddressData().get(COMPANY_NAME) != null ? StringUtility.toUpperCase(consignee.getAddressData().get(COMPANY_NAME).toString()) : null;
                shipmentContainer.consigneeAddress1 = consignee.getAddressData().get(ADDRESS1) != null ? consignee.getAddressData().get(ADDRESS1).toString() : null;
                shipmentContainer.consigneeAddress2 = consignee.getAddressData().get(ADDRESS2) != null ? consignee.getAddressData().get(ADDRESS2).toString() : null;
                shipmentContainer.consigneeCountry = consignee.getAddressData().get(COUNTRY) != null ? consignee.getAddressData().get(COUNTRY).toString() : null;
                shipmentContainer.consigneeZip = consignee.getAddressData().get(ZIP_POST_CODE) != null ? consignee.getAddressData().get(ZIP_POST_CODE).toString() : null;
                if (consignee.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
                    List<String> rawData = getAddressList(StringUtility.convertToString(consignee.getAddressData().containsKey(PartiesConstants.RAW_DATA)));
                    shipmentContainer.setConsigneeAddressFreeText(rawData.stream().map(StringUtility::toUpperCase).toList());
                }
            }

            PartiesModel notify = shipment.getAdditionalDetails().getNotifyParty();
            if(notify != null && notify.getAddressData() != null) {
                shipmentContainer.notifyCompanyName = notify.getAddressData().get(COMPANY_NAME) != null ? notify.getAddressData().get(COMPANY_NAME).toString() : null;
                shipmentContainer.notifyAddress1 = notify.getAddressData().get(ADDRESS1) != null ? notify.getAddressData().get(ADDRESS1).toString() : null;
                shipmentContainer.notifyAddress2 = notify.getAddressData().get(ADDRESS2) != null ? notify.getAddressData().get(ADDRESS2).toString() : null;
                shipmentContainer.notifyCountry = notify.getAddressData().get(COUNTRY) != null ? notify.getAddressData().get(COUNTRY).toString() : null;
                shipmentContainer.notifyZip = notify.getAddressData().get(ZIP_POST_CODE) != null ? notify.getAddressData().get(ZIP_POST_CODE).toString() : null;
                if (notify.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
                    List<String> rawData = getAddressList(StringUtility.convertToString(notify.getAddressData().containsKey(PartiesConstants.RAW_DATA)));
                    shipmentContainer.setNotifyPartyAddressFreeText(rawData.stream().map(StringUtility::toUpperCase).toList());
                }
            }

            shipmentContainer.setShipmentContainers(shipment.getContainersList().stream()
                    .map(i -> jsonHelper.convertValue(i, ShipmentContainers.class))
                    .toList());

            shipmentContainer.setConsigneeAddressFreeText(getPartyAddress(shipment.getConsignee()));
            shipmentContainer.setConsignerAddressFreeText(getPartyAddress(shipment.getConsigner()));
            if (shipment.getAdditionalDetails() != null)
                shipmentContainer.setNotifyPartyAddressFreeText(getPartyAddress(shipment.getAdditionalDetails().getNotifyParty()));

            shipmentContainers.add(shipmentContainer);
        }
        return shipmentContainers;
    }

    public List<ShipmentResponse> getShipmentResponse(List<ShipmentModel> shipments) {
        List<ShipmentResponse> shipmentResponses = new ArrayList<>();
        if (shipments == null) {
            return shipmentResponses;
        }

        for(var shipment : shipments) {
            ShipmentResponse response = new ShipmentResponse();
            PartiesModel consigner = shipment.getConsigner();
            PartiesModel consignee = shipment.getConsignee();

            response.houseBill = shipment.getHouseBill();
            response.masterBill = shipment.getMasterBill();
            if(consigner != null && consigner.getAddressData() != null) {
                response.consignerCompanyName = consigner.getAddressData().get(COMPANY_NAME) != null ? consigner.getAddressData().get(COMPANY_NAME).toString() : null;
                response.consignerLocalName = consigner.getAddressData().get(ReportConstants.LOCAL_NAME) != null ? consigner.getAddressData().get(ReportConstants.LOCAL_NAME).toString() : null;
            }
            if(consignee != null && consignee.getAddressData() != null) {
                response.consigneeCompanyName =  consignee.getAddressData().get(COMPANY_NAME) != null ? consignee.getAddressData().get(COMPANY_NAME).toString() : null;
                response.consigneeLocalName = consignee.getAddressData().get(ReportConstants.LOCAL_NAME) != null ? consignee.getAddressData().get(ReportConstants.LOCAL_NAME).toString() : null;
            }
            response.weight = shipment.getWeight();
            response.weightUnit = shipment.getWeightUnit();
            response.setConsigner(getPartyAddress(shipment.getConsigner()));
            response.setConsignee(getPartyAddress(shipment.getConsignee()));

            response.setConsigneeAddressFreeText(getPartyAddress(shipment.getConsigner()));
            response.setConsignerAddressFreeText(getPartyAddress(shipment.getConsignee()));
            response.setNotifyPartyAddressFreeText(getPartyAddress(shipment.getAdditionalDetails().getNotifyParty()));
            response.description = StringUtility.toUpperCase(shipment.getGoodsDescription());

            response.hsnNumber = shipment.getAdditionalDetails().getHsnNumber() != null ? shipment.getAdditionalDetails().getHsnNumber().toString() : null;
            response.totalPacks = getTotalPacks(shipment);
            response.freightOverseasCurrency = shipment.getFreightOverseasCurrency();

            shipmentResponses.add(response);
        }
        return shipmentResponses;
    }

    public static Long getTotalPacks(ShipmentModel shipmentDetails){
        long sum = 0L;
        if (shipmentDetails == null || shipmentDetails.getPackingList() == null)
            return sum;
        for (var packs : shipmentDetails.getPackingList()) {
            sum += Long.parseLong(packs.getPacks());
        }
        return sum;
    }


    private List<String> getPartyAddress(PartiesModel party) {
        if(party != null && party.getAddressData() != null) {
            return ReportHelper.getOrgAddress(
                    getValueFromMap(party.getAddressData(),COMPANY_NAME),
                    getValueFromMap(party.getAddressData(),ADDRESS1),
                    getValueFromMap(party.getAddressData(),ADDRESS2),
                    ReportHelper.getCityCountry(
                            getValueFromMap(party.getAddressData(),CITY),
                            getValueFromMap(party.getAddressData(),COUNTRY)
                    ),
                    getValueFromMap(party.getAddressData(),ZIP_POST_CODE),
                    getValueFromMap(party.getAddressData(),STATE)
            );
        }

        return List.of();
    }
    private boolean IsStringNullOrEmpty(String s){
        return s == null || s.isEmpty();
    }

    public Boolean getIsHbl(ShipmentModel shipmentModel) {
        if(shipmentModel.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
            if(shipmentModel.getDirection().equalsIgnoreCase(EXP)) {
                Long entityId = shipmentModel.getId();
                List<Awb> awbList = awbDao.findByShipmentId(entityId);
                String entityType = (Objects.equals(shipmentModel.getJobType(), Constants.SHIPMENT_TYPE_DRT)) ? Constants.DMAWB : Constants.HAWB;
                if (awbList != null && !awbList.isEmpty()) {
                    if(awbList.get(0).getAwbShipmentInfo().getEntityType().equalsIgnoreCase(entityType))
                        return false;
                }
                // TODO- Throw Exception that AWB is not generated
            }
            return false;
        } else if (shipmentModel.getDirection().equalsIgnoreCase(EXP)) {
            return true;
        }
        return false;
    }

    public List<String> createUnLocoRequestFromShipmentModel(ShipmentModel shipmentModel) {
        List<String> request = new ArrayList<>();
        if (Objects.isNull(shipmentModel)) return request;

        if (!Objects.isNull(shipmentModel.getAdditionalDetails())) {
            request.add(shipmentModel.getAdditionalDetails().getPaidPlace());
            request.add(shipmentModel.getAdditionalDetails().getPlaceOfIssue());
            request.add(shipmentModel.getAdditionalDetails().getPlaceOfSupply());
        }

        if (!Objects.isNull(shipmentModel.getCarrierDetails())) {
            request.add(shipmentModel.getCarrierDetails().getOriginPort());
            request.add(shipmentModel.getCarrierDetails().getDestinationPort());
            request.add(shipmentModel.getCarrierDetails().getOrigin());
            request.add(shipmentModel.getCarrierDetails().getDestination());
        }
        return request.stream().filter(Objects::nonNull).toList();
    }

    public List<String> createUnLocoRequestFromConsolidation(ConsolidationModel consolidationModel) {
        List<String> request = new ArrayList<>();
        if (Objects.isNull(consolidationModel)) return request;

        if (!Objects.isNull(consolidationModel.getCarrierDetails())) {
            request.add(consolidationModel.getCarrierDetails().getOriginPort());
            request.add(consolidationModel.getCarrierDetails().getDestinationPort());
            request.add(consolidationModel.getCarrierDetails().getOrigin());
            request.add(consolidationModel.getCarrierDetails().getDestination());
        }
        return request.stream().filter(Objects::nonNull).toList();
    }

    public List<MasterListRequest> createMasterListsRequestFromShipment(ShipmentModel shipmentModel) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(shipmentModel)) return request;
        request.add(createMasterListRequest(MasterDataType.PAYMENT, shipmentModel.getPaymentTerms()));
        request.add(createMasterListRequest(MasterDataType.SERVICE_MODE, shipmentModel.getServiceType()));
        request.add(createMasterListRequest(MasterDataType.TRANSPORT_MODE, shipmentModel.getTransportMode()));
        request.add(createMasterListRequest(MasterDataType.CUSTOM_SHIPMENT_TYPE, shipmentModel.getDirection()));
        request.add(createMasterListRequest(MasterDataType.PACKS_UNIT, shipmentModel.getPacksUnit()));
        request.add(createMasterListRequest(MasterDataType.VOLUME_UNIT, shipmentModel.getVolumeUnit()));
        request.add(createMasterListRequest(MasterDataType.WEIGHT_UNIT, shipmentModel.getNetWeightUnit()));
        if (!Objects.isNull(shipmentModel.getAdditionalDetails())) {
            request.add(createMasterListRequest(MasterDataType.RELEASE_TYPE, shipmentModel.getAdditionalDetails().getReleaseType()));
        }
        return request;
    }

    public List<MasterListRequest> createMasterListsRequestFromConsole(ConsolidationModel consolidationModel) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(consolidationModel)) return request;
        request.add(createMasterListRequest(MasterDataType.PAYMENT, consolidationModel.getPayment()));
        if (consolidationModel.getAllocations() != null) {
            request.add(createMasterListRequest(MasterDataType.VOLUME_UNIT, consolidationModel.getAllocations().getVolumeUnit()));
            request.add(createMasterListRequest(MasterDataType.WEIGHT_UNIT, consolidationModel.getAllocations().getWeightUnit()));
        }
        return request;
    }

    public List<MasterListRequest> createMasterListsRequestFromHbl(Hbl hbl) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(hbl)) return request;
        request.add(createMasterListRequest(MasterDataType.VOLUME_UNIT, hbl.getHblData().getCargoGrossVolumeUnit()));
        request.add(createMasterListRequest(MasterDataType.WEIGHT_UNIT, hbl.getHblData().getCargoGrossWeightUnit()));
        return request;
    }
    public List<MasterListRequest> createMasterListsRequestFromUnLocoMap(Map<String, UnlocationsResponse> unlocationsMap) {
        List<MasterListRequest> request = new ArrayList<>();
        for (String key : unlocationsMap.keySet())
            request.add(createMasterListRequest(MasterDataType.COUNTRIES, unlocationsMap.get(key).getCountry()));
        return request;
    }


    public MasterListRequest createMasterListRequest (MasterDataType itemType, String itemValue) {
        if (StringUtility.isEmpty(itemValue)) return null;
        return MasterListRequest.builder().ItemType(itemType.getDescription()).ItemValue(itemValue).build();
    }

    public Map<Integer, Map<String, MasterData>> fetchInBulkMasterList(MasterListRequestV2 requests) {
        Map<Integer, Map<String, MasterData>> dataMap = new HashMap<>();
        if(requests.getMasterListRequests() != null && !requests.getMasterListRequests().isEmpty()) {
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
            List<MasterData> masterLists = jsonHelper.convertValueToList(response.entities, MasterData.class);
            masterLists.forEach(masterData -> {
                dataMap.putIfAbsent(masterData.getItemType(), new HashMap<>());
                dataMap.get(masterData.getItemType()).put(masterData.getItemValue(), masterData);
            });
        }
        return dataMap;
    }

    public void populateHasContainerFields(ShipmentModel shipmentModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if ((!dictionary.containsKey(SHIPMENT_CONTAINERS) || dictionary.get(SHIPMENT_CONTAINERS) == null) && shipmentModel.getContainersList() != null && shipmentModel.getContainersList().size() > 0) {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_HAS_CONTAINERS, true);
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ContainerModel containerModel : shipmentModel.getContainersList()) {
                ShipmentContainers shipmentContainers = getShipmentContainer(containerModel);
                String shipContJsonString = jsonHelper.convertToJson(shipmentContainers);
                Map<String, Object> shipContJson = jsonHelper.convertJsonToMap(shipContJsonString);
                if(shipContJson.containsKey(ReportConstants.GROSS_VOLUME) && shipContJson.get(ReportConstants.GROSS_VOLUME) != null)
                    shipContJson.put(ReportConstants.GROSS_VOLUME, ConvertToVolumeNumberFormat(shipContJson.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
                if (shipContJson.containsKey(ReportConstants.GROSS_WEIGHT) && shipContJson.get(ReportConstants.GROSS_WEIGHT) != null)
                    shipContJson.put(ReportConstants.GROSS_WEIGHT, ConvertToWeightNumberFormat(shipContJson.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
                if (shipContJson.containsKey(ReportConstants.NET_WEIGHT) && shipContJson.get(ReportConstants.NET_WEIGHT) != null)
                    shipContJson.put(ReportConstants.NET_WEIGHT, ConvertToWeightNumberFormat(new BigDecimal(shipContJson.get(ReportConstants.NET_WEIGHT).toString())));
                if (shipContJson.containsKey(MIN_TEMP) && shipContJson.get(MIN_TEMP) != null)
                    shipContJson.put(MIN_TEMP, ConvertToWeightNumberFormat(new BigDecimal(String.valueOf(shipContJson.get(MIN_TEMP)))));
                if (shipContJson.containsKey(MAX_TEMP) && shipContJson.get(MAX_TEMP) != null)
                    shipContJson.put(MAX_TEMP, ConvertToWeightNumberFormat(new BigDecimal(String.valueOf(shipContJson.get(MAX_TEMP)))));
                if(shipContJson.containsKey(NO_OF_PACKAGES) && shipContJson.get(ReportConstants.NO_OF_PACKAGES) != null)
                    shipContJson.put(ReportConstants.NO_OF_PACKAGES, GetDPWWeightVolumeFormat(new BigDecimal(String.valueOf(shipContJson.get(NO_OF_PACKAGES))), 0, v1TenantSettingsResponse));
                valuesContainer.add(shipContJson);
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
        }
        else {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_HAS_CONTAINERS, false);
        }
    }

    // Populates packing details fields in the source dictionary
    // can return List<Map<String, Object>> packing Dictionary, keeping it void for now
    public List<Map<String, Object>> getPackingDetails(ShipmentModel shipment, Map<String, Object> dictionary) {
        if(shipment.getPackingList() == null || shipment.getPackingList().size() == 0) {
            dictionary.put(HAS_PACK_DETAILS, false);
            return null;
        }

        List<Map<String, Object>> packsDictionary = new ArrayList<>();
        Map<String, EntityTransferCommodityType> commodityTypeMap = new HashMap<>();
        try{
            List<String> commodityCodes = shipment.getPackingList().stream().map(PackingModel::getCommodity).toList();
            if(!commodityCodes.isEmpty())
                commodityTypeMap = masterDataUtils.fetchInBulkCommodityTypes(commodityCodes);
        } catch (Exception e) {
            log.error("Error while ");
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        for(var pack : shipment.getPackingList()) {
            Map<String, Object> dict = new HashMap<>();
            if(pack.getCommodity() != null) {
                dict.put(COMMODITY_DESC, pack.getCommodity());
                if(commodityTypeMap != null && commodityTypeMap.containsKey(pack.getCommodity()))
                    dict.put(COMMODITY_DESC_NAME, commodityTypeMap.get(pack.getCommodity()).getDescription());
            }
            if(pack.getWeight() != null){
                dict.put(WEIGHT_AND_UNIT_PACKS, String.format(REGEX_S_S, ConvertToWeightNumberFormat(pack.getWeight(), v1TenantSettingsResponse),
                        pack.getWeightUnit()));
            }
            if(pack.getVolume() != null){
                dict.put(VOLUME_AND_UNIT_PACKS, String.format(REGEX_S_S, ConvertToVolumeNumberFormat(pack.getVolume(), v1TenantSettingsResponse),
                        pack.getVolumeUnit()));
            }
            if (pack.getVolumeWeight() != null) {
                dict.put(V_WEIGHT_AND_UNIT_PACKS, String.format(REGEX_S_S, ConvertToWeightNumberFormat(pack.getVolumeWeight(), v1TenantSettingsResponse),
                        pack.getVolumeWeightUnit()));
            }
            if (shipment.getPickupDetails() != null && shipment.getPickupDetails().getActualPickupOrDelivery() != null) {
                dict.put(LOADED_DATE, ConvertToDPWDateFormat(shipment.getPickupDetails().getActualPickupOrDelivery()));
            }
            if(pack.getCommodityGroup() != null) {
                MasterData commodity = getMasterListData(MasterDataType.COMMODITY_GROUP, pack.getCommodityGroup());
                if(!Objects.isNull(commodity))
                    dict.put(PACKS_COMMODITY_GROUP, commodity.getItemDescription());
            }
            if(pack.getPacks() != null) {
                dict.put(PACKS, GetDPWWeightVolumeFormat(new BigDecimal(pack.getPacks()), 0, v1TenantSettingsResponse) );
            }
            if (pack.getPacksType() != null)
            {
                dict.put(ReportConstants.SHIPMENT_PACKING_PACKS_PACKSTYPE, pack.getPacksType());
            }

            dict.put(SHIPMENT_PACKING_LENGTH, GetDPWWeightVolumeFormat(pack.getLength(), 0, v1TenantSettingsResponse));
            dict.put(SHIPMENT_PACKING_LENGTH_UNIT, pack.getLengthUnit());
            dict.put(SHIPMENT_PACKING_WIDTH, GetDPWWeightVolumeFormat(pack.getWidth(), 0, v1TenantSettingsResponse));
            dict.put(SHIPMENT_PACKING_WIDTH_UNIT, pack.getWidthUnit());
            dict.put(SHIPMENT_PACKING_HEIGHT, GetDPWWeightVolumeFormat(pack.getHeight(), 0, v1TenantSettingsResponse));
            dict.put(SHIPMENT_PACKING_HEIGHT_UNIT, pack.getHeightUnit());
            dict.put(CHARGEABLE, ConvertToWeightNumberFormat(pack.getChargeable(), v1TenantSettingsResponse));
            dict.put(ChargeableUnit, pack.getChargeableUnit());
            dict.put(HS_CODE, pack.getHSCode());
            dict.put(DESCRIPTION, pack.getGoodsDescription());
            dict.put(IsDG, false);
            if(pack.getHazardous() != null && pack.getHazardous().equals(true)){
                var dgSubstanceRow = masterDataUtils.fetchDgSubstanceRow(pack.getDGSubstanceId());
                dict.put(DG_SUBSTANCE, dgSubstanceRow.ProperShippingName);
                dict.put(DG_CLASS, pack.getDGClass());
                dict.put(CLASS_DIVISION, dgSubstanceRow.ClassDivision);
                dict.put(UNID_NO, pack.getUNDGContact());
                dict.put(DANGEROUS_GOODS, "HAZARDOUS");
                dict.put(IsDG, true);
                dict.put(AirUNNumber, pack.getUnNumberAir());
                dict.put(AirDGClass, pack.getDgClassAir());
                dict.put(AirDGClassDescription, pack.getDgClassAirDescription());
            } else {
                dict.put(DG_SUBSTANCE, "");
                dict.put(DG_CLASS, "");
                dict.put(CLASS_DIVISION, "");
                dict.put(UNID_NO, "");
                dict.put(DANGEROUS_GOODS, "General");
            }

            if(pack.getIsTemperatureControlled() != null && pack.getIsTemperatureControlled().equals(true)) {
                dict.put(MIN_TEMP, pack.getMinTemp());
                dict.put(MAX_TEMP, pack.getMaxTemp());
                dict.put(MIN_TEMP_UNIT, pack.getMinTempUnit());
                dict.put(MAX_TEMP_UNIT, pack.getMaxTempUnit());
            } else {
                dict.put(MIN_TEMP, "");
                dict.put(MAX_TEMP, "");
                dict.put(MIN_TEMP_UNIT, "");
                dict.put(MAX_TEMP_UNIT, "");
            }
            packsDictionary.add(dict);
        }

        dictionary.put(HAS_PACK_DETAILS, true);
        dictionary.put(PACKS_DETAILS, packsDictionary);
        return packsDictionary;
    }
    public void populateBillChargesFields(ShipmentModel shipment, Map<String, Object> dictionary) {
        List<BillingResponse> billingsList = null;
        try {
            billingsList = getBillingData(shipment.getGuid());
        }
        catch (Exception e) { }
        List<BillChargesResponse> charges = new ArrayList<>();
        BillingResponse billRow = null;
        if(billingsList != null && billingsList.size() > 0) {
            billRow = billingsList.get(0);
            for(BillingResponse billingResponse : billingsList) {
                List<BillChargesResponse> billChargesResponses = getBillChargesData(billingResponse.getGuid());
                if(billChargesResponses != null) {
                    for (BillChargesResponse charge : billChargesResponses) {
                        charges.add(charge);
                    }
                }
            }
        }

        dictionary.put(ReportConstants.BILL_REMARKS, billRow != null ? billRow.getRemarks() : "");
        List<BillChargesResponse> originalChargesRows = new ArrayList<>();
        List<BillChargesResponse> copyChargesRows = new ArrayList<>();
        dictionary.put(AS_AGREED, false);
        dictionary.put(COPY_AS_AGREED, false);
        var v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        String chargesApply = shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getBLChargesDisplay() : null;

        if (!Objects.isNull(chargesApply) && chargesApply.equals("AGR")) {
            dictionary.put(AS_AGREED, true);
            dictionary.put(COPY_AS_AGREED, true);
        } else if (!Objects.isNull(chargesApply) && (chargesApply.equals("CPP") || chargesApply.equals("CAL") || chargesApply.equals("CCL"))) {
            dictionary.put(AS_AGREED, true);
        }

        if (Objects.isNull(chargesApply) || chargesApply.equals("NON")) {
            dictionary.put(HAS_CHARGES, false);
        } else {
            dictionary.put(HAS_CHARGES, true);
            getChargeRows(originalChargesRows, copyChargesRows, charges, chargesApply);
        }
        dictionary.put(CHARGES_SMALL, originalChargesRows);

        if(originalChargesRows != null && originalChargesRows.size() > 0)
        {
            List<Map<String, Object>> values = new ArrayList<>();
            for (BillChargesResponse billChargesResponse : originalChargesRows) {
                values.add(jsonHelper.convertValue(billChargesResponse, new TypeReference<>() {}));
            }
            for (Map<String, Object> v: values) {
                if(v.containsKey(OVERSEAS_SELL_AMOUNT) && v.get(OVERSEAS_SELL_AMOUNT) != null) {
                    v.put(OVERSEAS_SELL_AMOUNT, AmountNumberFormatter.Format(new BigDecimal(StringUtility.convertToString(v.get(OVERSEAS_SELL_AMOUNT))), StringUtility.convertToString(v.get("OverseasSellCurrency")), v1TenantSettingsResponse));
                };
            }
            dictionary.put(CHARGES_SMALL, values);
        }
        dictionary.put(COPY_CHARGES, copyChargesRows);
    }
    private void getChargeRows(List<BillChargesResponse> originalChargesRows, List<BillChargesResponse> copyChargesRows, List<BillChargesResponse> charges, String type) {
        List<BillChargesResponse> prepaid = charges.stream().filter(x -> !Strings.isNullOrEmpty(x.getPaymentType()) && x.getPaymentType().equals("PPD")).toList();
        List<BillChargesResponse> collect = charges.stream().filter(x -> !Strings.isNullOrEmpty(x.getPaymentType()) && x.getPaymentType().equals("CCX")).toList();

        switch (type)
        {
            case "CPP":
                copyChargesRows.addAll(prepaid);
                break;
            case "CAL":
                copyChargesRows.addAll(charges);
                break;

            case "PPD":
                originalChargesRows.addAll(prepaid);
                copyChargesRows.addAll(prepaid);
                break;

            case "SHW":
                originalChargesRows.addAll(collect);
                copyChargesRows.addAll(collect);
                break;
            case "ALL":
                originalChargesRows.addAll(charges);
                copyChargesRows.addAll(charges);
                break;

            case "CCL":
                copyChargesRows.addAll(collect);
                break;
            default:
                break;

        }
    }
    public void populateShipmentOrganizationsLL(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation) {
        var languageCode = UserContext.getUser().getLanguageCode();
        List<AddressTranslationRequest.OrgAddressCode> orgAddressCodeList = new ArrayList<>();
        if(!Objects.isNull(shipmentDetails.getClient()) && !Strings.isNullOrEmpty(shipmentDetails.getClient().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getClient().getOrgCode(), shipmentDetails.getClient().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getConsigner()) && !Strings.isNullOrEmpty(shipmentDetails.getConsigner().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getConsigner().getOrgCode(), shipmentDetails.getConsigner().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getConsignee()) && !Strings.isNullOrEmpty(shipmentDetails.getConsignee().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getConsignee().getOrgCode(), shipmentDetails.getConsignee().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getAdditionalDetails()) && !Objects.isNull(shipmentDetails.getAdditionalDetails().getNotifyParty()) && !Strings.isNullOrEmpty(shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode(), shipmentDetails.getAdditionalDetails().getNotifyParty().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getPickupDetails()) && !Objects.isNull(shipmentDetails.getPickupDetails().getSourceDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode(), shipmentDetails.getPickupDetails().getSourceDetail().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getDeliveryDetails()) && !Objects.isNull(shipmentDetails.getDeliveryDetails().getDestinationDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode(), shipmentDetails.getDeliveryDetails().getDestinationDetail().getAddressCode()));

        if(Strings.isNullOrEmpty(languageCode) || orgAddressCodeList.isEmpty()){
            return;
        }
        AddressTranslationRequest request = AddressTranslationRequest.builder()
                .OrgAddressCodeList(orgAddressCodeList)
                .LanguageCode(languageCode)
                .build();
        AddressTranslationListResponse response = null;
        try {
            response = v1Service.getAddressTranslation(request);
        } catch (Exception ex) { }
        Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap = new HashMap<>();
        if(!Objects.isNull(response) && !Objects.isNull(response.getAddressTranslationList()) && !response.getAddressTranslationList().isEmpty()){
           orgVsAddressTranslationMap =
                    response.getAddressTranslationList().stream().collect(Collectors.toMap(obj -> obj.getOrgCode() + "_" + obj.getAddressCode(), obj -> obj));
        }
        if(!Objects.isNull(shipmentDetails.getClient()) && !Strings.isNullOrEmpty(shipmentDetails.getClient().getOrgCode())){
            String orgCode = shipmentDetails.getClient().getOrgCode();
            String addressCode = shipmentDetails.getClient().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(CLIENT_LL, address.getOrgName());
                dictionary.put(CLIENT_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
//                throw new ValidationException("Translation not available for Client Organization");
                orgWithoutTranslation.add("Client");
            }
        }
        if(!Objects.isNull(shipmentDetails.getConsigner()) && !Strings.isNullOrEmpty(shipmentDetails.getConsigner().getOrgCode())){
            String orgCode = shipmentDetails.getConsigner().getOrgCode();
            String addressCode = shipmentDetails.getConsigner().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(CONSIGNER_LL, address.getOrgName());
                dictionary.put(CONSIGNER_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
//                throw new ValidationException("Translation not available for Consigner Organization");
                orgWithoutTranslation.add("Consigner");
            }
        }
        if(!Objects.isNull(shipmentDetails.getConsignee()) && !Strings.isNullOrEmpty(shipmentDetails.getConsignee().getOrgCode())){
            String orgCode = shipmentDetails.getConsignee().getOrgCode();
            String addressCode = shipmentDetails.getConsignee().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(CONSIGNEE_LL, address.getOrgName());
                dictionary.put(CONSIGNEE_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
//                throw new ValidationException("Translation not available for Consignee Organization");
                orgWithoutTranslation.add("Consignee");
            }
        }
        if(!Objects.isNull(shipmentDetails.getAdditionalDetails()) && !Objects.isNull(shipmentDetails.getAdditionalDetails().getNotifyParty()) && !Strings.isNullOrEmpty(shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode())){
            String orgCode = shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode();
            String addressCode = shipmentDetails.getAdditionalDetails().getNotifyParty().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(NOTIFY_PARTY_LL, address.getOrgName());
                dictionary.put(NOTIFY_PARTY_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
//                throw new ValidationException("Translation not available for Notify Party Organization");
                orgWithoutTranslation.add("Notify Party");
            }
        }
        if(!Objects.isNull(shipmentDetails.getPickupDetails()) && !Objects.isNull(shipmentDetails.getPickupDetails().getSourceDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode())){
            String orgCode = shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode();
            String addressCode = shipmentDetails.getPickupDetails().getSourceDetail().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(PICKUP_FROM_LL, address.getOrgName());
                dictionary.put(PICKUP_FROM_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
//                throw new ValidationException("Translation not available for PickupFrom Organization");
                orgWithoutTranslation.add("PickupFrom");
            }
        }
        if(!Objects.isNull(shipmentDetails.getDeliveryDetails()) && !Objects.isNull(shipmentDetails.getDeliveryDetails().getDestinationDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode())){
            String orgCode = shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode();
            String addressCode = shipmentDetails.getDeliveryDetails().getDestinationDetail().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(DELIVERY_TO_LL, address.getOrgName());
                dictionary.put(DELIVERY_TO_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
//                throw new ValidationException("Translation not available for DeliveryTo Organization");
                orgWithoutTranslation.add("DeliveryTo");
            }
        }
    }

    public String GetChargeTypeDescriptionLL(String chargeCode, List<String> chargeTypesWithoutTranslation) {
        var languageCode = UserContext.getUser().getLanguageCode();
        if(Strings.isNullOrEmpty(languageCode) || Strings.isNullOrEmpty(chargeCode)){
            return null;
        }
        NPMFetchMultiLangChargeCodeRequest request = NPMFetchMultiLangChargeCodeRequest.builder()
                .key(chargeCode)
                .lang(languageCode)
                .key_type("charge_code_desc")
                .build();
        try {
            NPMFetchLangChargeCodeResponse response = npmServiceAdapter.fetchMultiLangChargeCode(CommonRequestModel.buildRequest(request));
            if(Objects.isNull(response) || StringUtility.isEmpty(response.getTranslation())) {
//                throw new ValidationException("Translation not available for Charge Type Description for Charge Code: " + chargeCode);
                chargeTypesWithoutTranslation.add(chargeCode);
                return null;
            }
            return response.getTranslation();
        } catch (Exception ex) {
            throw new ValidationException("NPM service response failed for ChargeType translation due to: " + ex.getMessage());
        }
    }

    public void HandleTranslationErrors(Boolean printWithoutTranslation, List<String> orgWithoutTranslation, List<String> chargeTypesWithoutTranslation) {
        if(! Boolean.TRUE.equals(printWithoutTranslation)) {
            // Throw validation exception
            StringBuilder errorMessage = new StringBuilder();
            if(! orgWithoutTranslation.isEmpty()) {
                String failedOrgs = String.join(" ,", orgWithoutTranslation);
                errorMessage.append(String.format("Translation not available for orgs : %s", failedOrgs));
                errorMessage.append("\n");
            }
            if(! chargeTypesWithoutTranslation.isEmpty()) {
                String failedChargeType = String.join(" ,", chargeTypesWithoutTranslation);
                errorMessage.append(String.format("Translation not available for Charge codes : %s", failedChargeType));
            }
            if(errorMessage.length() != 0) {
                throw new TranslationException(errorMessage.toString());
            }
        }
    }

    public void SetContainerCount(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        var shipmentContainerList = shipmentModel.getContainersList();
        if (!shipmentContainerList.isEmpty()) {
            StringBuilder containerEtcCount = new StringBuilder(String.format("ETC %d CNTR", countAllContainers(shipmentContainerList) - 1));
            StringBuilder containerTypeValues = new StringBuilder(StringUtility.getEmptyString());
            var containerNumbers = shipmentContainerList.stream().filter(x -> !Objects.isNull(x.getContainerNumber()))
                    .map(ContainerModel::getContainerNumber).toList();
            String containerNumber = "";
            if (!containerNumbers.isEmpty()) {
                containerNumber = containerNumbers.get(0);
            }
            dictionary.put(CONTAINER_NUMBER_WITH_ETC_COUNT, String.format("%s %s", containerNumber, containerEtcCount));

            var containerCodeMap = shipmentContainerList.stream().filter(x -> !Objects.isNull(x.getContainerCode()))
                    .collect(Collectors.groupingBy(ContainerModel::getContainerCode));

            for (String containerCode : containerCodeMap.keySet()) {
                long containerCount = containerCodeMap.get(containerCode).stream().mapToLong(ContainerModel::getContainerCount).sum();
                containerTypeValues.append(String.format("%s * %d", containerCode, containerCount));
            }
            dictionary.put(CONTAINER_COUNT_WITH_ETC_COUNT, containerTypeValues);
        }

    }

    public static String checkCreditLimitDocs(String key){
        switch (key) {
            case ReportConstants.HOUSE_BILL:
                return Constants.HBL_PRINT;
            case ReportConstants.DELIVERY_ORDER:
                return Constants.DO_PRINT;
            case ReportConstants.HAWB:
                return Constants.HAWB_PRINT;
            case ReportConstants.MAWB:
                return Constants.MAWB_PRINT;
            default:
                return null;
        }
    }
    public Long countAllContainers(List<ContainerModel> containerModels) {
        Long count = 0l;
        for(ContainerModel model : containerModels) {
            count += model.getContainerCount();
        }
        return count;
    }

    public void populateTenantFields(Map<String, Object> dictionary, TenantModel tenantModel) {
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTNAME, tenantModel.getTenantName());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTADDRESS1, tenantModel.getAddress1());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTADDRESS2, tenantModel.getAddress2());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTEMAIL, tenantModel.getEmail());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTCITY, tenantModel.getCity());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTSTATE, tenantModel.getState());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTCOUNTRY, tenantModel.getCountry());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTCONTACTPHONE, tenantModel.getPhone());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTMOBILE, tenantModel.getMobile());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTZIPPOSTCODE, tenantModel.getZipPostCode());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTURL, tenantModel.getWebsiteUrl());
    }
    public Pair<BigDecimal, String> getTotalWeightManifest(List<ShipmentModel> shipments)
    {
        if (Objects.isNull(shipments) || shipments.isEmpty())
            return Pair.of(BigDecimal.ZERO, null);
        String weightUnit = null;
        BigDecimal totalWeight = BigDecimal.ZERO;

        for(var shipment: shipments)
        {
            if (!Strings.isNullOrEmpty(shipment.getWeightUnit()) && !Objects.isNull(shipment.getWeight()))
            {
                if (weightUnit == null)
                {
                    weightUnit = shipment.getWeightUnit();
                }

                if (!shipment.getWeightUnit().equals(weightUnit)) return Pair.of(BigDecimal.ZERO, null);
                totalWeight = totalWeight.add(shipment.getWeight());
            }
        }
        return Pair.of(totalWeight, weightUnit);
    }
    public Pair<BigDecimal, String> getTotalVolumeManifest(List<ShipmentModel> shipments)
    {
        if (Objects.isNull(shipments) || shipments.isEmpty())
            return Pair.of(BigDecimal.ZERO, null);
        String volumeUnit = null;
        BigDecimal totalVolume = BigDecimal.ZERO;

        for(var shipment: shipments)
        {
            if (!Strings.isNullOrEmpty(shipment.getVolumeUnit()) && !Objects.isNull(shipment.getVolume()))
            {
                if (volumeUnit == null)
                {
                    volumeUnit = shipment.getVolumeUnit();
                }

                if (!shipment.getVolumeUnit().equals(volumeUnit))
                    return Pair.of(BigDecimal.ZERO, null);
                totalVolume = totalVolume.add(shipment.getVolume());
            }
        }
        return Pair.of(totalVolume, volumeUnit);
    }
    public Pair<BigDecimal, String> getTotalPacksManifest(List<ShipmentModel> shipments)
    {
        if (Objects.isNull(shipments) || shipments.isEmpty())
            return Pair.of(BigDecimal.ZERO, null);
        String packsUnit = null;
        BigDecimal totalPacks = BigDecimal.ZERO;

        for(var shipment: shipments)
        {
            if (!Strings.isNullOrEmpty(shipment.getPacksUnit()) && shipment.getNoOfPacks() != null)
            {
                if (packsUnit == null)
                {
                    packsUnit = shipment.getPacksUnit();
                }

                if (!shipment.getPacksUnit().equals(packsUnit))
                    return Pair.of(BigDecimal.ZERO, null);
                totalPacks = totalPacks.add(BigDecimal.valueOf(shipment.getNoOfPacks()));
            }
        }
        return Pair.of(totalPacks, packsUnit);
    }

    public void populateAddress(Map<String, Object> addressData, Map<String, Object> dictionary, String prefix) {
        dictionary.put(prefix + COMPANY_NAME, getValueFromMap(addressData, COMPANY_NAME));
        dictionary.put(prefix + ADDRESS1, getValueFromMap(addressData, ADDRESS1));
        dictionary.put(prefix + ADDRESS2, getValueFromMap(addressData, ADDRESS2));
        dictionary.put(prefix + EMAIL, getValueFromMap(addressData, EMAIL));
        dictionary.put(prefix + CITY, getValueFromMap(addressData, CITY));
        dictionary.put(prefix + STATE, getValueFromMap(addressData, STATE));
        dictionary.put(prefix + COUNTRY, getValueFromMap(addressData, COUNTRY));
        dictionary.put(prefix + CONTACT_PHONE, getValueFromMap(addressData, CONTACT_PHONE));
        dictionary.put(prefix + MOBILE, getValueFromMap(addressData, MOBILE));
        dictionary.put(prefix + ZIP_POST_CODE, getValueFromMap(addressData, ZIP_POST_CODE));
    }

    public void populateRaKcData(Map<String, Object> dictionary, ShipmentModel shipmentModel) {
        Parties partiesModelSendingAgent = shipmentModel.getAdditionalDetails().getExportBroker() != null ? modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class) : null;
        Parties partiesModelReceivingAgent = shipmentModel.getAdditionalDetails().getImportBroker() != null ? modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class) : null;
        Parties consignor = shipmentModel.getConsigner() != null ? modelMapper.map(shipmentModel.getConsigner(), Parties.class) : null;

        List<Parties> parties = Arrays.asList(
                partiesModelSendingAgent,
                partiesModelReceivingAgent,
                consignor
        );

        OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(parties);
        Map<String, Object> addressSendingAgent = null;
        Map<String, Object> addressReceivingAgent = null;
        Map<String, Object> addressConsignorAgent = null;

        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
        if(partiesModelSendingAgent != null) {
            addressSendingAgent = addressMap.get(partiesModelSendingAgent.getOrgCode() + "#" + partiesModelSendingAgent.getAddressCode());
        }
        if(partiesModelReceivingAgent != null) {
            addressReceivingAgent = addressMap.get(partiesModelReceivingAgent.getOrgCode() + "#" + partiesModelReceivingAgent.getAddressCode());
        }
        if(consignor != null) {
            addressConsignorAgent = addressMap.get(consignor.getOrgCode() + "#" + consignor.getAddressCode());
        }

        processAgent(addressSendingAgent, dictionary, ONE, ORIGIN_AGENT);
        processAgent(addressReceivingAgent, dictionary, ONE, DESTINATION_AGENT);
        processAgent(addressConsignorAgent, dictionary, TWO, CONSIGNOR_AGENT);

        if (shipmentModel.getAdditionalDetails() != null) {
            AdditionalDetailModel additionalDetailModel = shipmentModel.getAdditionalDetails();
            if(additionalDetailModel.getExemptionCodes() != null) {
                dictionary.put(EXEMPTION_CARGO, additionalDetailModel.getExemptionCodes());
            }
            if(additionalDetailModel.getScreeningStatus() != null && !additionalDetailModel.getScreeningStatus().isEmpty()) {
                Set<String> screeningCodes = additionalDetailModel.getScreeningStatus().stream().collect(Collectors.toSet());
                if(screeningCodes.contains(Constants.AOM)){
                    screeningCodes.remove(Constants.AOM);
                    String aomString = Constants.AOM;
                    if(additionalDetailModel.getAomFreeText() != null) {
                        aomString =  aomString + " (" + additionalDetailModel.getAomFreeText() + ")";
                    }
                    screeningCodes.add(aomString);
                    dictionary.put(SCREENING_CODES, screeningCodes);
                } else {
                    dictionary.put(SCREENING_CODES, screeningCodes);
                }

            }
        }

        if(shipmentModel.getSecurityStatus() != null ) {
            dictionary.put(CONSIGNMENT_STATUS, shipmentModel.getSecurityStatus());
        }
    }

    private String getDate(Map<String, Object> agent) {
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        return ConvertToDPWDateFormat(LocalDateTime.parse(StringUtility.convertToString(agent.get(KCRA_EXPIRY))), v1TenantSettingsResponse.getDPWDateFormat());
    }

    private void processAgent(Map<String, Object> agent, Map<String, Object> dictionary, String type, String agentType) {
        if(agent != null) {
            var raKcType = Boolean.TRUE.equals(agent.get(REGULATED_AGENT))? ONE : "";
            raKcType = Boolean.TRUE.equals(agent.get(KNOWN_CONSIGNOR))? TWO : raKcType;
            if(Objects.equals(raKcType, type)) {
                if(type.equals(ONE)) {
                    dictionary.put(agentType + TYPE, RA);
                } else if(type.equals(TWO)) {
                    dictionary.put(agentType + TYPE, KC);
                }

                if(StringUtility.isNotEmpty(StringUtility.convertToString(agent.get(KCRA_NUMBER)))) {
                    if(type.equals(ONE)) {
                        dictionary.put(agentType + RA_NUMBER, agent.get(KCRA_NUMBER));
                    } else if(type.equals(TWO)) {
                        dictionary.put(agentType + KC_NUMBER, agent.get(KCRA_NUMBER));
                    }
                }

                if(StringUtility.isNotEmpty(StringUtility.convertToString(agent.get(KCRA_EXPIRY)))) {
                    if(type.equals(ONE)) {
                        dictionary.put(agentType + RA_EXPIRY, getDate(agent));
                    } else if(type.equals(TWO)) {
                        dictionary.put(agentType + KC_EXPIRY, getDate(agent));
                    }
                }
            }
        }
    }

    public void populateIGMInfo(ShipmentModel shipment, Map<String, Object> dictionary) {
        if (Objects.isNull(shipment))
            return;
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        var additionalDetails = shipment.getAdditionalDetails();
        if (v1TenantSettingsResponse.isEnableIGMDetails() && Objects.equals(shipment.getDirection(), Constants.IMP) && !Objects.isNull(additionalDetails)) {
            dictionary.put(ReportConstants.IGM_FILE_DATE, additionalDetails.getIGMFileDate());
            dictionary.put(ReportConstants.IGM_FILE_NO, additionalDetails.getIGMFileNo());
            dictionary.put(ReportConstants.IGM_INWARD_DATE, additionalDetails.getIGMInwardDate());
            dictionary.put(ReportConstants.INWARD_DATE_TIME, additionalDetails.getInwardDateAndTime());
            dictionary.put(ReportConstants.LINE_NUMBER, additionalDetails.getLineNumber());
            dictionary.put(ReportConstants.SUB_LINE_NUMBER, additionalDetails.getSubLineNumber());
            if (Boolean.TRUE.equals(additionalDetails.getIsInland())) {
                dictionary.put(ReportConstants.IS_INLAND, Boolean.TRUE.equals(additionalDetails.getIsInland()) ? "Yes" : "No");
                dictionary.put(ReportConstants.SMTPIGM_DATE, additionalDetails.getSMTPIGMDate());
                dictionary.put(ReportConstants.SMTPIGM_NUMBER, additionalDetails.getSMTPIGMNumber());
                dictionary.put(ReportConstants.LOCAL_LINE_NUMBER, additionalDetails.getLocalLineNumber());
            }
        }

    }

    public void validateAirDGCheck(ShipmentModel shipmentModel) {
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) &&
                Boolean.TRUE.equals(shipmentModel.getContainsHazardous()) && shipmentModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
           boolean dgPack = false;
           if(shipmentModel.getPackingList() != null && !shipmentModel.getPackingList().isEmpty()) {
               for (PackingModel packingModel: shipmentModel.getPackingList()) {
                   if(Boolean.TRUE.equals(packingModel.getHazardous())) {
                       dgPack = true;
                       break;
                   }
               }
           }
           if(!dgPack) {
               throw new ValidationException("The shipment is marked as DG but does not contain any DG packages. Please add DG packs before printing.");
           }
        }
    }

    private static boolean isDgUser() {
        return UserContext.isDgUser();
    }

    public void validateAirDGCheckConsolidations(ConsolidationModel consolidationModel) {
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) &&
                Boolean.TRUE.equals(consolidationModel.getHazardous()) && consolidationModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !isDgUser()) {
                throw new ValidationException("You do not have permission to print the freight documents.");
        }
    }

    public void validateAirDGCheckShipments(ShipmentModel shipmentModel) {
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) &&
                Boolean.TRUE.equals(shipmentModel.getContainsHazardous()) && shipmentModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !isDgUser()) {
                throw new ValidationException("You do not have permission to print the freight documents.");
        }
    }

    public void updateShipmentWeightAndPack(Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(dictionary.get(ReportConstants.SHIPMENTS) != null) {
            List<Map<String, Object>> values = jsonHelper.convertValue(dictionary.get(ReportConstants.SHIPMENTS), new TypeReference<>() {});
            if (Objects.isNull(values)) values = new ArrayList<>();
            values.forEach(v -> {
                if (v.containsKey(ReportConstants.WEIGHT))
                    v.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(new BigDecimal(v.get(ReportConstants.WEIGHT).toString()), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.TOTAL_PACKS))
                    v.put(ReportConstants.TOTAL_PACKS, ConvertToVolumeNumberFormat(new BigDecimal(v.get(ReportConstants.TOTAL_PACKS).toString()), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.DESCRIPTION))
                    v.put(ReportConstants.DESCRIPTION, StringUtility.toUpperCase(StringUtility.convertToString(v.get(ReportConstants.DESCRIPTION))));
            });
            dictionary.put(ReportConstants.SHIPMENTS, values);
        }
    }

    public void addTransportInstructionTags(Map<String, Object> dictionary, ShipmentModel shipmentModel) {
        if (Objects.isNull(shipmentModel.getPickupDeliveryDetailsInstructions()) || shipmentModel.getPickupDeliveryDetailsInstructions().isEmpty())
            return;

        Optional<PickupDeliveryDetailsModel> transportInstruction = shipmentModel.getPickupDeliveryDetailsInstructions().stream().filter(pickupDeliveryDetailsModel -> pickupDeliveryDetailsModel.getId().equals(shipmentModel.getTransportInstructionId())).findFirst();
        if (transportInstruction.isEmpty())
            return;
        var ti = transportInstruction.get();
        Optional<PartiesModel> exportAgent = ti.getPartiesModels()!= null ? ti.getPartiesModels().stream().filter(Objects::nonNull).filter(c -> c.getType().contains("Export Agent")).findFirst() : Optional.empty();
        Optional<PartiesModel> importAgent = ti.getPartiesModels()!= null ? ti.getPartiesModels().stream().filter(Objects::nonNull).filter(c -> c.getType().contains("Import Agent")).findFirst() : Optional.empty();
        Optional<PartiesModel> deliveryAgent = ti.getPartiesModels()!=null ? ti.getPartiesModels().stream().filter(Objects::nonNull).filter(c -> c.getType().contains("Delivery Agent")).findFirst() : Optional.empty();
        dictionary.put(TI_INSTRUCTIONTYPE, ti.getType());
        dictionary.put(TI_DROPMODE, ti.getDropMode());

        dictionary.put(ReportConstants.TI_EXPORT_AGENT, exportAgent.isPresent() && exportAgent.get().getOrgData() != null ? exportAgent.get().getOrgData().get("FullName") : "");
        dictionary.put(ReportConstants.TI_EXPORT_AGENT_ADDRESS, exportAgent.isPresent() ? getFormattedAddress(exportAgent.get()) : "");
        dictionary.put(ReportConstants.TI_EXPORT_AGENT_CONTACT,  exportAgent.isPresent() ? ReportHelper.getValueFromMap(exportAgent.get().getAddressData(), ReportConstants.CONTACT_PHONE) : "");

        dictionary.put(ReportConstants.TI_IMPORT_AGENT , importAgent.isPresent() && importAgent.get().getOrgData() != null ? importAgent.get().getOrgData().get("FullName") : "");
        dictionary.put(ReportConstants.TI_IMPORT_AGENT_ADDRESS, importAgent.isPresent() ? getFormattedAddress(importAgent.get()) : "");
        dictionary.put(ReportConstants.TI_IMPORT_AGENT_CONTACT,  importAgent.isPresent() ? ReportHelper.getValueFromMap(importAgent.get().getAddressData(), ReportConstants.CONTACT_PHONE) : "");

        dictionary.put(TI_DELIVERY_AGENT, deliveryAgent.isPresent() && deliveryAgent.get().getOrgData() != null ? deliveryAgent.get().getOrgData().get("FullName") : "");
        dictionary.put(TI_DELIVERY_AGENT_ADDRESS, deliveryAgent.isPresent() ? getFormattedAddress(deliveryAgent.get()) : "");
        dictionary.put(TI_DELIVERY_AGENT_CONTACT, deliveryAgent.isPresent() ? ReportHelper.getValueFromMap(deliveryAgent.get().getAddressData(), ReportConstants.CONTACT_PHONE) : "");

        dictionary.put(TI_TRANSPORTCOMPANY, getPartyAddress(ti.getTransporterDetail()));
        dictionary.put(TI_PICKUPFROM, getFormattedAddress(ti.getSourceDetail()));
        dictionary.put(TI_DELIVERTO, getFormattedAddress(ti.getDestinationDetail()));
        dictionary.put(TI_TRANSPORTCOMPANYADDRESS, getFormattedAddress(ti.getTransporterDetail()));
        dictionary.put(TI_TRANSPORTCOMPANYCONTACT, ti.getTransporterDetail() != null ? ReportHelper.getValueFromMap(ti.getTransporterDetail().getAddressData(), EMAIL) : "");
        dictionary.put(TI_PICKUPFROMADDRESS, getFormattedAddress(ti.getSourceDetail()));
        dictionary.put(TI_PICKUPFROMCONTACT, ti.getSourceDetail() != null ? ReportHelper.getValueFromMap(ti.getSourceDetail().getAddressData(), EMAIL) : "");
        dictionary.put(TI_DELIVERTOADDRESS, getFormattedAddress(ti.getDestinationDetail()));
        dictionary.put(TI_DELIVERTOCONTACT, ti.getDestinationDetail() != null ? ReportHelper.getValueFromMap(ti.getDestinationDetail().getAddressData(), EMAIL) : "");
        dictionary.put(TI_REMARKS, ti.getRemarks());
        dictionary.put(TI_PORTTRANSPORTADVISED, ti.getPortTransportAdvised());
        dictionary.put(TI_REQUIREDBY, ti.getRequiredBy());
        dictionary.put(TI_ESTIMATEDPICKUP, ConvertToDPWDateFormat(ti.getEstimatedPickup()));
        dictionary.put(TI_ESTIMATEDDELIVERY, ConvertToDPWDateFormat(ti.getEstimatedDelivery()));
        dictionary.put(TI_ACTUALPICKUP, ConvertToDPWDateFormat(ti.getActualPickup()));
        dictionary.put(TI_ACTUALDELIVERY, ConvertToDPWDateFormat(ti.getActualDelivery()));
        dictionary.put(TI_PICKUP_GATEIN, ConvertToDPWDateFormat(ti.getPickupGateIn()));
        dictionary.put(TI_PICKUP_GATEOUT, ConvertToDPWDateFormat(ti.getPickupGateOut()));
        dictionary.put(TI_DELIVERY_GATEIN, ConvertToDPWDateFormat(ti.getDeliveryGateIn()));
        dictionary.put(TI_DELIVERY_GATEOUT, ConvertToDPWDateFormat(ti.getDeliveryGateOut()));

    }
}

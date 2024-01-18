package com.dpw.runner.shipment.services.ReportingService.Reports;

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
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchMultiLangChargeCodeRequest;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import com.dpw.runner.shipment.services.dto.v1.request.AddressTranslationRequest;
import com.dpw.runner.shipment.services.dto.v1.response.AddressTranslationListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
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
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;

public abstract class IReport {

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
    private MasterDataUtils masterDataUtils;
    @Autowired
    CustomKeyGenerator keyGenerator;

    public abstract Map<String, Object> getData(Long id);
    abstract IDocumentModel getDocumentModel(Long id);
    abstract Map<String, Object> populateDictionary(IDocumentModel documentModel);

    public ShipmentContainers getShipmentContainer(ContainerModel row)
    {
        ShipmentContainers ship = new ShipmentContainers();
        ship.ContainerNumber = row.getContainerNumber();
        ship.SealNumber = row.getSealNumber();
        ship.NoofPackages = row.getNoOfPackages();
        if(row.getPacks() != null && !row.getPacks().isEmpty())
            ship.ShipmentPacks = Long.valueOf(row.getPacks());
        ship.ShipmentPacksUnit = row.getPacksType();
        ship.GrossWeight = getRoundedBigDecimal(row.getGrossWeight(),2, RoundingMode.HALF_UP);
        ship.GrossWeightUnit = row.getGrossWeightUnit();
        ship.TareWeight =  getRoundedBigDecimal(row.getTareWeight(),2, RoundingMode.HALF_UP);
        ship.TareWeightUnit = row.getTareWeightUnit();
        ship.Measurement = getRoundedBigDecimal(row.getMeasurement(),2, RoundingMode.HALF_UP);
        ship.MeasurementUnit = row.getMeasurementUnit();
        ship.GrossVolume = getRoundedBigDecimal(row.getGrossVolume(),2, RoundingMode.HALF_UP);
        ship.GrossVolumeUnit = row.getGrossVolumeUnit();
        ship.ContainerTypeCode = row.getContainerCode();
        ship.ContainerCount = row.getContainerCount();
        ship.ShipmentMarksnNums = row.getMarksNums();
        ship.NetWeight = getRoundedBigDecimal(row.getNetWeight(),2, RoundingMode.HALF_UP);
        ship.NetWeightUnit = row.getNetWeightUnit();
        ship.MinTemp = getRoundedBigDecimal(row.getMinTemp(),2, RoundingMode.HALF_UP);
        ship.MinTempUnit = row.getMinTempUnit();
        ship.ShipmentHblDeliveryMode = row.getHblDeliveryMode();
        ship.DescriptionOfGoods = row.getDescriptionOfGoods();
        ship.CarrierSealNumber = row.getCarrierSealNumber();
        ship.CustomsSealNumber = row.getCustomsSealNumber();
        ship.ShipperSealNumber = row.getShipperSealNumber();

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

            if(requests.size() > 0) {
                MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
                masterListRequestV2.setMasterListRequests(requests);
                masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));
                Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
                masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);
            }
            ship.VolumeUnitDescription = getMasterListItemDesc(ship.GrossVolumeUnit);
            ship.WeightUnitDescription = getMasterListItemDesc(ship.GrossWeightUnit);
            ship.PacksUnitDescription = getMasterListItemDesc(ship.ShipmentPacksUnit);
            ship.VGMWeight = ship.GrossWeight.add(ship.TareWeight);
        } catch (Exception ignored) { }
        CommodityResponse commodityResponse = getCommodity(row.getCommodityCode());
        if (commodityResponse != null)
            ship.CommodityDescription = commodityResponse.getDescription();
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
        shipmentContainer.BL_ContainerType = blObjectContainer.getContainerType();
        shipmentContainer.BL_SealNumber = blObjectContainer.getSealNumber();
        if (blObjectContainer.getContainerGrossWeight() != null)
            shipmentContainer.BL_GrossWeight = blObjectContainer.getContainerGrossWeight().setScale(2, RoundingMode.HALF_UP);
        else
            shipmentContainer.BL_GrossWeight = BigDecimal.ZERO;
        shipmentContainer.BL_GrossWeightUnit = blObjectContainer.getContainerGrossWeightUnit();
        if (blObjectContainer.getContainerGrossVolume() != null)
            shipmentContainer.BL_GrossVolume = blObjectContainer.getContainerGrossVolume().setScale(2, RoundingMode.HALF_UP);
        else
            shipmentContainer.BL_GrossVolume = BigDecimal.ZERO;
        shipmentContainer.BL_GrossVolumeUnit = blObjectContainer.getContainerGrossVolumeUnit();
        shipmentContainer.BL_NoofPackages = blObjectContainer.getNoOfPackages();
        shipmentContainer.BL_CarrierSealNumber = blObjectContainer.getCarrierSealNumber();
        shipmentContainer.BL_ContainerNumber = blObjectContainer.getContainerNumber();
        shipmentContainer.BL_ContainerDescription = blObjectContainer.getContainerDesc();
        shipmentContainer.BL_PackageUnit = blObjectContainer.getPackageUnit();
    }

    public void populateShipmentFields(ShipmentModel shipment, Boolean isHBL, Map<String, Object> dictionary)
    {
        if (shipment == null) {
            return;
        }

        PickupDeliveryDetailsModel pickup = shipment.getPickupDetails();
        PickupDeliveryDetailsModel delivery = shipment.getDeliveryDetails();

        PartiesModel shipmentClient = shipment.getClient();
        PartiesModel shipmentConsignee = shipment.getConsignee();
        PartiesModel shipmentConsigner = shipment.getConsigner();
        AdditionalDetailModel additionalDetails = new AdditionalDetailModel();
        if(shipment.getAdditionalDetails() != null) {
            additionalDetails = shipment.getAdditionalDetails();
        }
        // UnLocations Master-data
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(shipment);
        Map<String, UnlocationsResponse> unlocationsMap = getLocationData(new HashSet<>(unlocoRequests));
        // Master lists Master-data
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromShipment(shipment);
        masterListRequest.addAll(createMasterListsRequestFromUnLocoMap(unlocationsMap));
        Map<String, MasterData> masterListsMap = fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).collect(Collectors.toList())).build());
        PartiesModel shipmentNotify = additionalDetails.getNotifyParty();

        UnlocationsResponse pol = unlocationsMap.get(shipment.getCarrierDetails().getOriginPort());
        UnlocationsResponse pod = unlocationsMap.get(shipment.getCarrierDetails().getDestinationPort());
        UnlocationsResponse origin = unlocationsMap.get(shipment.getCarrierDetails().getOrigin());
        UnlocationsResponse destination = unlocationsMap.get(shipment.getCarrierDetails().getDestination());
        UnlocationsResponse paidPlace = unlocationsMap.get(shipment.getAdditionalDetails().getPaidPlace());
        UnlocationsResponse placeOfSupply = unlocationsMap.get(shipment.getAdditionalDetails().getPlaceOfSupply());
        UnlocationsResponse placeOfIssue = unlocationsMap.get(shipment.getAdditionalDetails().getPlaceOfIssue());

        dictionary.put(ReportConstants.MASTER_BILL,shipment.getMasterBill());
        dictionary.put(ReportConstants.HOUSE_BILL,shipment.getHouseBill());
        dictionary.put(ReportConstants.VESSEL_NAME, shipment.getCarrierDetails().getVessel());
        dictionary.put(ReportConstants.VOYAGE,shipment.getCarrierDetails().getVoyage());
        dictionary.put(ReportConstants.POL_CODE, shipment.getCarrierDetails().getOriginPort());
        dictionary.put(ReportConstants.POD_CODE, shipment.getCarrierDetails().getDestinationPort());
        dictionary.put(ReportConstants.POD_COUNTRY, pod != null ? pod.getCountry() : null);
        dictionary.put(ReportConstants.POL_COUNTRY, pol != null ? pol.getCountry() : null);
        dictionary.put(ReportConstants.POL_PORTNAME, pol != null ? pol.getPortName() : null);
        dictionary.put(ReportConstants.POD_PORTNAME, pod != null ? pod.getPortName() : null);
        dictionary.put(ReportConstants.CARRIER,shipment.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, pod != null ? pod.getName() : null);
        dictionary.put(ReportConstants.PORT_OF_LOADING, pol != null ? pol.getName() : null);
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.REFERENCE_NUMBER,shipment.getBookingReference());
        dictionary.put(ReportConstants.ORIGIN_NAME, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.ORIGIN, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.ORIGIN_COUNTRY, origin != null ? origin.getCountry() : null);
        dictionary.put(ReportConstants.DESCRIPTION,shipment.getGoodsDescription());
        dictionary.put(ReportConstants.SHIPMENT_TYPE,shipment.getDirection());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, shipment.getDirection() != null ? Character.toUpperCase(shipment.getDirection().charAt(0)) : null);
        Long containerCount = 0L;
        if (shipment.getContainersList().size() > 0) {
            for (ContainerModel container : shipment.getContainersList()) {
                if (container.getContainerCount() != null && container.getContainerCount() != 0) {
                    containerCount += container.getContainerCount();
                }
            }
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT, containerCount);
        dictionary.put(PICKUP_INSTRUCTION, shipment.getPickupDetails() != null ? shipment.getPickupDetails().getPickupDeliveryInstruction() : null);
        dictionary.put(DELIVERY_INSTRUCTIONS, shipment.getDeliveryDetails() != null ? shipment.getDeliveryDetails().getPickupDeliveryInstruction() : null);
        V1TenantSettingsResponse v1TenantSettingsResponse = getTenantSettings();
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
        dictionary.put(ReportConstants.DATE_OF_ISSUE, GenerateFormattedDate(additionalDetails.getDateOfIssue(), formatPattern));
        dictionary.put(SHIPMENT_DETAIL_DATE_OF_ISSUE, GenerateFormattedDate(additionalDetails.getDateOfIssue(), formatPattern));
        dictionary.put(ReportConstants.DATE_OF_RECEIPT, additionalDetails.getDateOfReceipt());

        dictionary.put(ReportConstants.INCO_TERM, shipment.getIncoterms());
        dictionary.put(ReportConstants.CHARGEABLE, shipment.getChargable());
        dictionary.put(ReportConstants.CHARGEABLE_UNIT, shipment.getChargeableUnit());
        dictionary.put(ReportConstants.TRANSPORT_MODE, shipment.getTransportMode());
        MasterData masterData = null;
        if (masterListsMap.containsKey(shipment.getTransportMode()))
            masterData = masterListsMap.get(shipment.getTransportMode());
        dictionary.put(ReportConstants.TRANSPORT_MODE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getTransportMode());
        if (masterListsMap.containsKey(shipment.getDirection()))
            masterData = masterListsMap.get(shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_TYPE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_NUMBER, shipment.getShipmentId());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, shipment.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.ADDITIONAL_TERMS, shipment.getAdditionalTerms());

        dictionary.put(ReportConstants.PACKS,shipment.getNoOfPacks());
        dictionary.put(ReportConstants.PACKS_UNIT,shipment.getPacksUnit());
        if (masterListsMap.containsKey(shipment.getPacksUnit()))
            masterData = masterListsMap.get(shipment.getPacksUnit());
        dictionary.put(ReportConstants.PACKS_UNIT_DESC, masterData != null && StringUtility.isNotEmpty(masterData.getItemDescription()) ? masterData.getItemDescription() : shipment.getPacksUnit());
        dictionary.put(ReportConstants.GROSS_WEIGHT,shipment.getWeight());
        dictionary.put(ReportConstants.GROSS_WEIGHT_UNIT,shipment.getWeightUnit());
        dictionary.put(ReportConstants.GROSS_VOLUME,shipment.getVolume());
        dictionary.put(ReportConstants.GROSS_VOLUME_UNIT,shipment.getVolumeUnit());

        dictionary.put(ReportConstants.DELIVERY_CFS, (delivery != null && !Objects.isNull(delivery.getSourceDetail()) && !Objects.isNull(delivery.getSourceDetail().getOrgData())) ? delivery.getSourceDetail().getOrgData().get(FULL_NAME) : null);
        dictionary.put(ReportConstants.PICKUP_CFS, (pickup != null && !Objects.isNull(pickup.getDestinationDetail()) && !Objects.isNull(pickup.getDestinationDetail().getOrgData()) )? pickup.getDestinationDetail().getOrgData().get(FULL_NAME) : null);
        dictionary.put(ReportConstants.MARKS_N_NUMS,shipment.getMarksNum());
        dictionary.put(ReportConstants.ORIGINALS,additionalDetails.getOriginal() == null ? 1 : additionalDetails.getOriginal());
        dictionary.put(ReportConstants.ORIGINAL_WORDS, "");
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

        if (Objects.equals(shipment.getPaymentTerms(), "PPD")) {
            dictionary.put(ReportConstants.AT, pol != null ? pol.getName() : null);
        } else if (Objects.equals(shipment.getPaymentTerms(), "CCX")) {
            dictionary.put(ReportConstants.AT, pod != null ? pod.getName() : null);
        }

        var array = new String[] {"" + dictionary.get("VesselName"), shipment.getCarrierDetails().getVoyage()};
        dictionary.put(ReportConstants.VESSEL_NAME_AND_VOYAGE, array[0] + " & " + array[1]);

        dictionary.put(ReportConstants.WAREHOUSE_NAME, additionalDetails.getWarehouseId());
        masterData = null;
        if(placeOfIssue != null && masterListsMap.containsKey(placeOfIssue.getCountry()))  {
            masterData = masterListsMap.get(placeOfIssue.getCountry());
        }
        dictionary.put(ReportConstants.ISSUEPLACECOUNTRYNAME, masterData != null ? masterData.getItemDescription() : null);

        if (!isHBL && (Objects.equals(shipment.getTransportMode(), "SEA") || Objects.equals(shipment.getTransportMode(), "ROA") || Objects.equals(shipment.getTransportMode(), "RF"))) {
            List<String> consigner = null;
            if(shipmentConsigner != null)
            {
                Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
                if(consignerAddress != null)
                {
                    consigner = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consignerAddress, COMPANY_NAME), getValueFromMap(consignerAddress, ADDRESS1),
                            getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                            getValueFromMap(consignerAddress,"Email"), getValueFromMap(consignerAddress, CONTACT_PHONE),
                            getValueFromMap(consignerAddress,ZIP_POST_CODE));
                    dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get(COMPANY_NAME));
                    dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get("ContactPerson"));
                    try {
                        dictionary.put(ReportConstants.ConsignerPhone, consignerAddress.get("ContactPhone"));
                        dictionary.put(ReportConstants.ConsignerFullName, shipmentConsigner.getOrgData().get("FullName"));
                    } catch (Exception ignored) { }
                }
                if(shipmentConsigner.getOrgData() != null)
                    dictionary.put(ReportConstants.CONSIGNER_LOCAL_NAME,shipmentConsigner.getOrgData().get("LocalName"));
            }
            List<String> consignee = null;
            if(shipmentConsignee != null)
            {
                Map<String, Object> consigneeAddress = shipmentConsignee.getAddressData();
                if(consigneeAddress != null)
                {
                    consignee = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consigneeAddress, COMPANY_NAME), getValueFromMap(consigneeAddress, ADDRESS1),
                            getValueFromMap(consigneeAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(consigneeAddress, CITY), getValueFromMap(consigneeAddress, COUNTRY)),
                            getValueFromMap(consigneeAddress,"Email"), getValueFromMap(consigneeAddress, CONTACT_PHONE),
                            getValueFromMap(consigneeAddress,ZIP_POST_CODE));
                    dictionary.put(ReportConstants.CONSIGNEE_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                    dictionary.put(ReportConstants.CONSIGNEE_CONTACT_PERSON,getValueFromMap(consigneeAddress,"ContactPerson"));
                }
                if(shipmentConsignee.getOrgData() != null)
                    dictionary.put(ReportConstants.CONSIGNEE_LOCAL_NAME, getValueFromMap(shipmentConsignee.getOrgData(),"LocalName"));
            }
            List<String> notify = null;
            if(shipmentNotify != null)
            {
                Map<String, Object> notifyAddress = shipmentNotify.getAddressData();
                if(notifyAddress != null)
                {
                    notify = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(notifyAddress, COMPANY_NAME), getValueFromMap(notifyAddress, ADDRESS1),
                            getValueFromMap(notifyAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(notifyAddress, CITY), getValueFromMap(notifyAddress, COUNTRY)),
                            getValueFromMap(notifyAddress,"Email"), getValueFromMap(notifyAddress, CONTACT_PHONE),
                            getValueFromMap(notifyAddress,ZIP_POST_CODE)
                                                                     );
                    dictionary.put(ReportConstants.NOTIFY_PARTY_NAME,getValueFromMap(notifyAddress, COMPANY_NAME));
                    dictionary.put(ReportConstants.NOTIFY_PARTY_CONTACT_PERSON,getValueFromMap(notifyAddress,"ContactPerson"));
                }
                if(shipmentNotify.getOrgData() != null)
                    dictionary.put(ReportConstants.NOTIFY_PARTY_LOCAL_NAME,getValueFromMap(shipmentNotify.getOrgData(),"LocalName"));
            }
            List<String> client = null;
            if(shipmentClient != null)
            {
                Map<String, Object> clientAddress = shipmentClient.getAddressData();
                if(clientAddress != null) {
                    client = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(clientAddress, COMPANY_NAME), getValueFromMap(clientAddress, ADDRESS1),
                            getValueFromMap(clientAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(clientAddress, CITY), getValueFromMap(clientAddress, COUNTRY)),
                            getValueFromMap(clientAddress, "Email"), getValueFromMap(clientAddress, CONTACT_PHONE),
                            getValueFromMap(clientAddress, ZIP_POST_CODE));
                    dictionary.put(ReportConstants.CLIENT_NAME, getValueFromMap(clientAddress, COMPANY_NAME));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_1, getValueFromMap(clientAddress, ADDRESS1));
                    dictionary.put(CLIENT_ADDRESS_COUNTRY, getValueFromMap(clientAddress, COUNTRY));
                    dictionary.put(CLIENT_ADDRESS_CITY, getValueFromMap(clientAddress, CITY));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_PHONE, getValueFromMap(clientAddress, CONTACT_PHONE));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_MOBILE, getValueFromMap(clientAddress, "Mobile"));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_CONTACT_PERSON, getValueFromMap(clientAddress, "ContactPerson"));
                }
            }

            dictionary.put(ReportConstants.CONSIGNER,consigner);
            dictionary.put(ReportConstants.CONSIGNEE,consignee);
            dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
            dictionary.put(ReportConstants.CONSIGNEE_FREETEXT,dictionary.get(ReportConstants.CONSIGNEE));
            dictionary.put(ReportConstants.CONSIGNER_FREETEXT,dictionary.get(ReportConstants.CONSIGNER));
            dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, dictionary.get(ReportConstants.NOTIFY_PARTY));
            dictionary.put(ReportConstants.CLIENT, client);
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList = new ArrayList<>();
            referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(ERN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(EXPORTER_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList = new ArrayList<>();
            referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(CEN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(CUSTOMS_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList = new ArrayList<>();
            referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(FRN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(FORWARDER_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
        }
        if(!Strings.isNullOrEmpty(shipment.getCarrierDetails().getShippingLine())){
            CarrierMasterData carrierData = getCarrier(shipment.getCarrierDetails().getShippingLine());
            if(!Objects.isNull(carrierData))
                dictionary.put(CARRIER_NAME, carrierData.getItemDescription());
        }
        if(!Objects.isNull(pickup) && !Objects.isNull(pickup.getTransporterDetail())){
            dictionary.put(PRE_CARRIAGE_PARTY, pickup.getTransporterDetail().getOrgData() != null &&
                    pickup.getTransporterDetail().getOrgData().containsKey("FullName") ?
                    pickup.getTransporterDetail().getOrgData().get("FullName") : "");
        }
        dictionary.put(ReportConstants.NO_OF_PACKAGES, shipment.getNoOfPacks());

        populateHasContainerFields(shipment, dictionary);
    }

    public ShipmentModel getShipment(Long Id)
    {
        ShipmentDetails shipmentDetails = shipmentDao.findById(Id).get();
        return modelMapper.map(shipmentDetails, ShipmentModel.class);
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

    public ShipmentSettingsDetails getShipmentSettings(Integer tenantId) {
        ShipmentSettingsDetails tenantSettingsRow = new ShipmentSettingsDetails();
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
        if (shipmentSettingsDetailsList != null && shipmentSettingsDetailsList.size() >= 1) {
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

        PartiesModel sendingAgent = consolidation.getSendingAgent();
        PartiesModel receivingAgent = consolidation.getReceivingAgent();
        PartiesModel creditor = consolidation.getCreditor();
        ArrivalDepartureDetailsModel arrivalDetails = consolidation.getArrivalDetails();
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
                dictionary.put(ReportConstants.SENDING_AGENT_NAME, addressData.get(COMPANY_NAME));
            }
            Map<String, Object> orgData = sendingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.SENDING_AGENT_LOCAL_NAME, orgData.get("LocalName"));
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
                dictionary.put(ReportConstants.RECEIVING_AGENT_NAME, addressData.get(COMPANY_NAME));
            }
            Map<String, Object> orgData = receivingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.RECEIVING_AGENT_LOCAL_NAME, orgData.get("LocalName"));
            }
        }

        List<String> exportAgentFreeTextAddress;
        List<String> importAgentFreeTextAddress;
        if (consolidation.getIsSendingAgentFreeTextAddress() != null && consolidation.getIsSendingAgentFreeTextAddress())
        {
            exportAgentFreeTextAddress = ReportHelper.getAddressList(consolidation.getSendingAgentFreeTextAddress());
        }
        else
        {
            exportAgentFreeTextAddress = exportAgentAddress;
        }

        if (consolidation.getIsReceivingAgentFreeTextAddress() != null && consolidation.getIsReceivingAgentFreeTextAddress())
        {
            importAgentFreeTextAddress = ReportHelper.getAddressList(consolidation.getReceivingAgentFreeTextAddress());
        }
        else
        {
            importAgentFreeTextAddress = importAgentAddress;
        }
        if (Objects.equals(consolidation.getShipmentType(), "EXP"))
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, exportAgentFreeTextAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, importAgentFreeTextAddress);
        }
        else
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, importAgentFreeTextAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, exportAgentFreeTextAddress);
        }

        dictionary.put(ReportConstants.SHIPMENT_TYPE, consolidation.getShipmentType());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, !StringUtility.isEmpty(consolidation.getShipmentType())
                ? consolidation.getShipmentType().toUpperCase().charAt(0) : null);

        dictionary.put(ReportConstants.CONSOL_VESSEL_NAME, consolidation.getCarrierDetails().getVessel());
        dictionary.put(ReportConstants.CONSOL_VOYAGE, consolidation.getCarrierDetails().getVoyage());
        if(creditor != null)
        {
            Map<String, Object> orgData = creditor.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.CREDITOR, Arrays.asList(orgData.get(FULL_NAME), creditor.getAddressData() != null ? creditor.getAddressData().get(ADDRESS1) : null));
                dictionary.put(ReportConstants.CREDITOR_LOCAL_NAME, orgData.get("LocalName"));
            }
        }
        dictionary.put(ReportConstants.CONSOL_ETA, consolidation.getCarrierDetails().getEta());
        dictionary.put(ReportConstants.CONSOL_ETD, consolidation.getCarrierDetails().getEtd());
        dictionary.put(ReportConstants.CONSOL_ATA, consolidation.getCarrierDetails().getAta());
        dictionary.put(ReportConstants.CONSOL_ATD, consolidation.getCarrierDetails().getAtd());

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
        dictionary.put(ReportConstants.CONSOL_FLIGHT_NUMBER, consolidation.getCarrierDetails().getFlightNumber());

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
    }

    public void populateBlFields(Hbl hbl, Map<String, Object> dictionary)
    {
        if (hbl == null) return;
        List<String> notify = null;
        if(hbl.getHblNotifyParty() != null && hbl.getHblNotifyParty().size() > 0) {
            HblPartyDto hblNotify = hbl.getHblNotifyParty().get(0);
            notify = ReportHelper.getOrgAddress(hblNotify.getName(), hblNotify.getAddress(), null, null, hblNotify.getEmail(), null);
        }
        dictionary.put(ReportConstants.BL_NOTIFY_PARTY, notify);
        if(notify != null)
            dictionary.put(ReportConstants.BL_NOTIFY_PARTY_CAPS, notify.stream().map(String::toUpperCase).collect(Collectors.toList()));
        HblDataDto hblDataDto = hbl.getHblData();
        List<String> consignor = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsignorName() : null, hblDataDto != null ? hblDataDto.getConsignorAddress() : null, null, null, null, null);
        List<String> consignee = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsigneeName() : null, hblDataDto != null ? hblDataDto.getConsigneeAddress() : null, null, null, null, null);
        dictionary.put(ReportConstants.CONSIGNER, consignor);
        dictionary.put(ReportConstants.CONSIGNEE, consignee);
        dictionary.put(ReportConstants.CONSIGNER_CAPS, consignor.stream().map(String::toUpperCase).collect(Collectors.toList()));
        dictionary.put(ReportConstants.CONSIGNEE_CAPS, consignee.stream().map(String::toUpperCase).collect(Collectors.toList()));
        dictionary.put(ReportConstants.HOUSE_BILL, hblDataDto.getHouseBill());
        dictionary.put(ReportConstants.BL_VESSEL_NAME, hblDataDto.getVesselName());
        dictionary.put(ReportConstants.BL_VOYAGE, hblDataDto.getVoyage());
        dictionary.put(ReportConstants.PORT_OF_LOADING, hblDataDto.getPortOfLoad());
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, hblDataDto.getPortOfDischarge());
        dictionary.put(ReportConstants.MARKS_N_NUMS, hblDataDto.getMarksAndNumbers());
        dictionary.put(ReportConstants.MARKS_N_NUMS_CAPS, hblDataDto.getMarksAndNumbers() != null ? hblDataDto.getMarksAndNumbers().toUpperCase() : null);
        dictionary.put(ReportConstants.PLACE_OF_RECEIPT, hblDataDto.getPlaceOfReceipt());
        dictionary.put(ReportConstants.PACKS, hblDataDto.getPackageCount());
        dictionary.put(ReportConstants.PACKS_UNIT, hblDataDto.getPackageType());
        dictionary.put(ReportConstants.DESCRIPTION, hblDataDto.getCargoDescription());
        dictionary.put(ReportConstants.DESCRIPTION_CAPS, hblDataDto.getCargoDescription() != null ? hblDataDto.getCargoDescription().toUpperCase() : null);
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, hblDataDto.getPlaceOfDelivery());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT, hblDataDto.getCargoNetWeight());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT_UNIT, hblDataDto.getCargoNetWeightUnit());
        dictionary.put(ReportConstants.FINAL_DESTINATION, hblDataDto.getFinalDestination());
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME, hblDataDto.getCargoGrossVolume());
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME_UNIT, hblDataDto.getCargoGrossVolumeUnit());
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT, hblDataDto.getCargoGrossWeight());
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT_UNIT, hblDataDto.getCargoGrossWeightUnit());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT, hblDataDto.getCargoNetWeight());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT_UNIT, hblDataDto.getCargoNetWeightUnit());
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_COUNT, hblDataDto.getPackageCount());
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_TYPE, hblDataDto.getPackageType());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY, hblDataDto.getQuantity());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY_CODE, hblDataDto.getQuantityCode());
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, hblDataDto.getPlaceOfDelivery());
        dictionary.put(ReportConstants.PLACE_OF_RECEIPT, hblDataDto.getPlaceOfReceipt());
        dictionary.put(ReportConstants.BL_COMMENTS, hblDataDto.getBlComments());
        dictionary.put(ReportConstants.BLComments, hblDataDto.getBlComments());
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT, hblDataDto.getDeliveryAgent());
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT_ADDRESS, hblDataDto.getDeliveryAgentAddress());
        dictionary.put(ReportConstants.BL_CARGO_TERMS_DESCRIPTION, hblDataDto.getCargoTermsDescription());
        dictionary.put(ReportConstants.BL_REMARKS_DESCRIPTION, hblDataDto.getBlRemarksDescription());
    }

    public void populateUserFields(UsersDto user, Map<String, Object> dictionary) {
        if (user == null) {
            return;
        }
        dictionary.put(ReportConstants.USER_FULLNAME, user.DisplayName);
        dictionary.put(ReportConstants.TENANT_NAME, user.TenantDisplayName);
        dictionary.put(ReportConstants.USER_NAME, user.Username);
        dictionary.put(PRINT_USER, user.Username.toUpperCase());
        dictionary.put(ReportConstants.USER_EMAIL, user.Email);
        dictionary.put(ReportConstants.TENANT_CURRENCY, user.CompanyCurrency);
    }

    public MasterData getMasterListData(MasterDataType type, String ItemValue)
    {
        if (ItemValue == null || StringUtility.isEmpty(ItemValue)) return null;
        MasterListRequest masterListRequest = MasterListRequest.builder().ItemType(type.getDescription()).ItemValue(ItemValue).build();
        MasterListRequestV2 masterListRequests = new MasterListRequestV2();
        masterListRequests.getMasterListRequests().add(masterListRequest);
        Object masterDataList = masterDataFactory.getMasterDataService().fetchMultipleMasterData(masterListRequests).getData();
        List<MasterData> masterData = new ArrayList<>();
        if (masterDataList != null) {
            for (Object data : (ArrayList<?>) masterDataList) {
                MasterData masterDataObject = modelMapper.map(data, MasterData.class);
                masterData.add(masterDataObject);
            }
        }
        if (masterData == null || masterData.isEmpty())
            return null;
        return masterData.get(0);
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
        Object carrierResponse = masterDataFactory.getMasterDataService().fetchCarrierMasterData(carrierListObject).getData();
        List<CarrierMasterData> carrierMasterData = jsonHelper.convertValueToList(carrierResponse, CarrierMasterData.class);
        if(carrierMasterData == null || carrierMasterData.isEmpty())
            return null;
        return carrierMasterData.get(0);
    }

    public List<ContainerCountByCode> getCountByContainerTypeCode(List<ShipmentContainers> commonContainers) {
        Map<String, Long> containerTypeCountMap = new HashMap<>();
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
            countByCode.ContainerCount = entry.getValue();
            containerCountByCode.add(countByCode);
        }
        return containerCountByCode;
    }

    public List<ContainerCountByCode> getCountByCommonContainerTypeCode(List<ContainerModel> commonContainers) {
        Map<String, Long> containerTypeCountMap = new HashMap<>();
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
            countByCode.ContainerCount = entry.getValue();
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
        if(awb != null && !awb.isEmpty())
            return awb.get(0);
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

    public static String twoDecimalPlacesFormat(String value)
    {
        if(StringUtility.isEmpty(value))
        {
            return value;
        }

        else
        {
            return String.format("##.00", value);
        }
    }

    public static String twoDecimalPlacesFormatDecimal(BigDecimal value)
    {
        if(value == null)
        {
            return "0.00";
        }
        return twoDecimalPlacesFormat(value.toString());
    }

    public DateTimeFormatter GetDPWDateFormatOrDefault()
    {
        V1TenantSettingsResponse v1TenantSettingsResponse = getTenantSettings();
        if(!CommonUtils.IsStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
            return DateTimeFormatter.ofPattern(v1TenantSettingsResponse.getDPWDateFormat());
        return DateTimeFormatter.ofPattern("MM/dd/yyyy");
    }

    public static DateTimeFormatter GetDPWDateFormatWithTime()
    {
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

    public static String ConvertToDPWDateFormatWithTime(LocalDateTime date)
    {
        String strDate = "";
        if (date != null)
        {
            strDate = date.format(GetDPWDateFormatWithTime());
        }
        return strDate;
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

    public UnlocationsResponse getUNLocRow(String UNLocCode) {
        if(UNLocCode == null || UNLocCode.isEmpty())
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "=",
                UNLocCode
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        Object unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            return unlocationsResponse.get(0);
        return null;
    }

    public V1TenantSettingsResponse getTenantSettings() {
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().retrieveTenantSettings();
        return modelMapper.map(dependentServiceResponse.getData(), V1TenantSettingsResponse.class);
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
                if(packing.getWeight() != null && IsStringNullOrEmpty(packing.getWeightUnit())) {
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
                if(packing.getVolume() != null && IsStringNullOrEmpty(packing.getVolumeUnit())) {
                    if(volumeUnit == null) {
                        volumeUnit = packing.getVolumeUnit();
                    }
                    if(!Objects.equals(packing.getWeightUnit(), volumeUnit))
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
        if(shipments == null)
            return shipmentContainers;

        for(var shipment : shipments) {
            ShipmentAndContainerResponse shipmentContainer = new ShipmentAndContainerResponse();

            shipmentContainer.hsnNumber = shipment.getAdditionalDetails().getHsnNumber() != null ?
                    shipment.getAdditionalDetails().getHsnNumber().toString(): null;
            shipmentContainer.houseBill = shipment.getHouseBill();
            shipmentContainer.masterBill = shipment.getMasterBill();

            PartiesModel consigner = shipment.getConsigner();
            if(consigner != null && consigner.getAddressData() != null) {
                shipmentContainer.consignerCompanyName = consigner.getAddressData().get(COMPANY_NAME) != null ? consigner.getAddressData().get(COMPANY_NAME).toString() : null;
                shipmentContainer.consignerAddress1 = consigner.getAddressData().get(ADDRESS1) != null ? consigner.getAddressData().get(ADDRESS1).toString() : null;
                shipmentContainer.consignerAddress2 = consigner.getAddressData().get(ADDRESS2) != null ? consigner.getAddressData().get(ADDRESS2).toString() : null;
                shipmentContainer.consignerCountry = consigner.getAddressData().get(COUNTRY) != null ? consigner.getAddressData().get(COUNTRY).toString() : null;
                shipmentContainer.consignerZip = consigner.getAddressData().get(ZIP_POST_CODE) != null ? consigner.getAddressData().get(ZIP_POST_CODE).toString() : null;
            }

            PartiesModel consignee = shipment.getConsignee();
            if(consignee != null && consignee.getAddressData() != null) {
                shipmentContainer.consigneeCompanyName = consignee.getAddressData().get(COMPANY_NAME) != null ? consignee.getAddressData().get(COMPANY_NAME).toString() : null;
                shipmentContainer.consigneeAddress1 = consignee.getAddressData().get(ADDRESS1) != null ? consignee.getAddressData().get(ADDRESS1).toString() : null;
                shipmentContainer.consigneeAddress2 = consignee.getAddressData().get(ADDRESS2) != null ? consignee.getAddressData().get(ADDRESS2).toString() : null;
                shipmentContainer.consigneeCountry = consignee.getAddressData().get(COUNTRY) != null ? consignee.getAddressData().get(COUNTRY).toString() : null;
                shipmentContainer.consigneeZip = consignee.getAddressData().get(ZIP_POST_CODE) != null ? consignee.getAddressData().get(ZIP_POST_CODE).toString() : null;
            }

            PartiesModel notify = shipment.getAdditionalDetails().getNotifyParty();
            if(notify != null && notify.getAddressData() != null) {
                shipmentContainer.notifyCompanyName = notify.getAddressData().get(COMPANY_NAME) != null ? notify.getAddressData().get(COMPANY_NAME).toString() : null;
                shipmentContainer.notifyAddress1 = notify.getAddressData().get(ADDRESS1) != null ? notify.getAddressData().get(ADDRESS1).toString() : null;
                shipmentContainer.notifyAddress2 = notify.getAddressData().get(ADDRESS2) != null ? notify.getAddressData().get(ADDRESS2).toString() : null;
                shipmentContainer.notifyCountry = notify.getAddressData().get(COUNTRY) != null ? notify.getAddressData().get(COUNTRY).toString() : null;
                shipmentContainer.notifyZip = notify.getAddressData().get(ZIP_POST_CODE) != null ? notify.getAddressData().get(ZIP_POST_CODE).toString() : null;
            }

            shipmentContainer.shipmentContainers = shipment.getContainersList()
                    .stream()
                    .map(i -> getShipmentContainer(
                            jsonHelper.convertValue(i, ContainerModel.class)
                    )).toList();

            shipmentContainer.consigneeAddressFreeText = getPartyAddress(shipment.getConsignee());
            shipmentContainer.consignerAddressFreeText = getPartyAddress(shipment.getConsigner());
            if (shipment.getAdditionalDetails() != null)
                shipmentContainer.notifyPartyAddressFreeText = getPartyAddress(shipment.getAdditionalDetails().getNotifyParty());

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

            response.HouseBill = shipment.getHouseBill();
            response.MasterBill = shipment.getMasterBill();
            if(consigner != null && consigner.getAddressData() != null) {
                response.ConsignerCompanyName = consigner.getAddressData().get(COMPANY_NAME) != null ? consigner.getAddressData().get(COMPANY_NAME).toString() : null;
                response.ConsignerLocalName = consigner.getAddressData().get(LOCAL_NAME) != null ? consigner.getAddressData().get(LOCAL_NAME).toString() : null;
            }
            if(consignee != null && consignee.getAddressData() != null) {
                response.ConsigneeCompanyName =  consignee.getAddressData().get(COMPANY_NAME) != null ? consignee.getAddressData().get(COMPANY_NAME).toString() : null;
                response.ConsigneeLocalName = consignee.getAddressData().get(LOCAL_NAME) != null ? consignee.getAddressData().get(LOCAL_NAME).toString() : null;
            }
            response.Weight = shipment.getWeight();
            response.WeightUnit = shipment.getWeightUnit();
            response.Consigner = getPartyAddress(shipment.getConsigner());
            response.Consignee =getPartyAddress(shipment.getConsignee());

            response.ConsigneeAddressFreeText = getPartyAddress(shipment.getConsigner());
            response.ConsignerAddressFreeText = getPartyAddress(shipment.getConsignee());
            response.NotifyPartyAddressFreeText = getPartyAddress(shipment.getAdditionalDetails().getNotifyParty());
//            response.Description = shipment.Description;

            response.HsnNumber = shipment.getAdditionalDetails().getHsnNumber() != null ? shipment.getAdditionalDetails().getHsnNumber().toString() : null;
            response.TotalPacks = getTotalPacks(shipment);

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
                String entityType = (shipmentModel.getJobType() == Constants.SHIPMENT_TYPE_DRT) ? Constants.DMAWB : Constants.HAWB;
                if (awbList != null && awbList.size() > 0) {
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
        return request.stream().filter(Objects::nonNull).collect(Collectors.toList());
    }

    public List<MasterListRequest> createMasterListsRequestFromShipment(ShipmentModel shipmentModel) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(shipmentModel)) return request;
        request.add(createMasterListRequest(MasterDataType.PAYMENT, shipmentModel.getPaymentTerms()));
        request.add(createMasterListRequest(MasterDataType.SERVICE_MODE, shipmentModel.getServiceType()));
        request.add(createMasterListRequest(MasterDataType.TRANSPORT_MODE, shipmentModel.getTransportMode()));
        request.add(createMasterListRequest(MasterDataType.CUSTOM_SHIPMENT_TYPE, shipmentModel.getDirection()));
        request.add(createMasterListRequest(MasterDataType.PACKS_UNIT, shipmentModel.getPacksUnit()));

        if (!Objects.isNull(shipmentModel.getAdditionalDetails())) {
            request.add(createMasterListRequest(MasterDataType.RELEASE_TYPE, shipmentModel.getAdditionalDetails().getReleaseType()));
        }
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

    public Map<String, MasterData> fetchInBulkMasterList(MasterListRequestV2 requests) {
        Map<String, MasterData> keyMasterDataMap = new HashMap<>();
        if(requests.getMasterListRequests() != null && requests.getMasterListRequests().size() > 0) {
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
            List<MasterData> masterLists = jsonHelper.convertValueToList(response.entities, MasterData.class);
            masterLists.forEach(masterData -> {
                keyMasterDataMap.put(masterData.getItemValue(), masterData);
            });
        }
        return keyMasterDataMap;
    }

    public void populateHasContainerFields(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        if (shipmentModel.getContainersList() != null && shipmentModel.getContainersList().size() > 0) {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_HAS_CONTAINERS, true);
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, shipmentModel.getContainersList());
        }
        else {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_HAS_CONTAINERS, false);
        }
    }

    // Populates packing details fields in the source dictionary
    // can return List<Map<String, Object>> packing Dictionary, keeping it void for now
    public void getPackingDetails(ShipmentModel shipment, Map<String, Object> dictionary) {
        if(shipment.getPackingList() == null || shipment.getPackingList().size() == 0) {
            dictionary.put(HAS_PACK_DETAILS, false);
            return;
        }

        List<Map<String, Object>> packsDictionary = (List<Map<String, Object>>) new HashMap<>();

        for(var pack : shipment.getPackingList()) {
            Map<String, Object> dict = new HashMap<>();
            if(pack.getCommodity() != null)
                dict.put(COMMODITY_DESC, pack.getCommodity());
            if(pack.getWeight() != null){
                dict.put(WEIGHT_AND_UNIT_PACKS, String.format("%s %s", twoDecimalPlacesFormat(pack.getWeight().toString()),
                        pack.getWeightUnit()));
            }
            if(pack.getVolume() != null){
                dict.put(VOLUME_AND_UNIT_PACKS, String.format("%s %s", twoDecimalPlacesFormat(pack.getVolume().toString()),
                        pack.getVolumeUnit()));
            }
            if (pack.getVolumeWeight() != null) {
                dict.put(V_WEIGHT_AND_UNIT_PACKS, String.format("%s %s", twoDecimalPlacesFormat(pack.getVolumeWeight().toString()),
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

            dict.put(SHIPMENT_PACKING_LENGTH, pack.getLength());
            dict.put(SHIPMENT_PACKING_LENGTH_UNIT, pack.getLengthUnit());
            dict.put(SHIPMENT_PACKING_WIDTH, pack.getWidth());
            dict.put(SHIPMENT_PACKING_WIDTH_UNIT, pack.getWidthUnit());
            dict.put(SHIPMENT_PACKING_HEIGHT, pack.getHeight());
            dict.put(SHIPMENT_PACKING_HEIGHT_UNIT, pack.getHeightUnit());
            dict.put(CHARGEABLE, pack.getChargeable());
            dict.put(ChargeableUnit, pack.getChargeableUnit());

            if(pack.getHazardous() != null && pack.getHazardous().equals(true)){
                var dgSubstanceRow = ReportHelper.fetchDgSubstanceRow(pack.getDGSubstanceId());
                dict.put(DG_SUBSTANCE, dgSubstanceRow.ProperShippingName);
                dict.put(DG_CLASS, pack.getDGClass());
                dict.put(CLASS_DIVISION, dgSubstanceRow.ClassDivision);
                dict.put(UNID_NO, pack.getUNDGContact());
                dict.put(DANGEROUS_GOODS, "HAZARDOUS");
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

    }
    public void populateShipmentOrganizationsLL(ShipmentModel shipmentDetails, Map<String, Object> dictionary) {
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
                throw new ValidationException("Translation not available for Client Organization");
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
                throw new ValidationException("Translation not available for Consigner Organization");
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
                throw new ValidationException("Translation not available for Consignee Organization");
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
                throw new ValidationException("Translation not available for Notify Party Organization");
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
                throw new ValidationException("Translation not available for PickupFrom Organization");
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
                throw new ValidationException("Translation not available for DeliveryTo Organization");
            }
        }
    }

    public String GetChargeTypeDescriptionLL(String chargeCode) {
        var languageCode = UserContext.getUser().getLanguageCode();
        if(Strings.isNullOrEmpty(languageCode) || Strings.isNullOrEmpty(chargeCode)){
            return null;
        }
        NPMFetchMultiLangChargeCodeRequest request = NPMFetchMultiLangChargeCodeRequest.builder()
                .key(chargeCode)
                .lang(languageCode)
                .key_type("charge_code_desc")
                .build();
        String translatedChargeTypeDescription = null;
        try {
            NPMFetchLangChargeCodeResponse response = npmServiceAdapter.fetchMultiLangChargeCode(CommonRequestModel.buildRequest(request));
            translatedChargeTypeDescription = response.getTranslation();
            if(Strings.isNullOrEmpty(translatedChargeTypeDescription)){
                throw new ValidationException("Translation not available for Charge Type Description for Charge Code: " + chargeCode);
            }
        } catch (Exception ex){
            throw new ValidationException("NPM service response failed for ChargeType translation due to: " + ex.getMessage());
        }
        return translatedChargeTypeDescription;
    }
}

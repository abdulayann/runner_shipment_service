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
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.combineStringsWithComma;

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
    MasterDataFactory masterDataFactory;

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
        CommodityResponse commodityResponse = getCommodity(row.getCommodityCode());
        if(commodityResponse != null)
            ship.CommodityDescription = commodityResponse.getDescription();
        return ship;
    }

    public void populateBLContainer(ShipmentContainers shipmentContainer, HblContainerDto blObjectContainer)
    {
        shipmentContainer.BL_ContainerType = blObjectContainer.getContainerType();
        shipmentContainer.BL_SealNumber = blObjectContainer.getSealNumber();
        shipmentContainer.BL_GrossWeight = blObjectContainer.getContainerGrossWeight().setScale(2, RoundingMode.HALF_UP);
        shipmentContainer.BL_GrossWeightUnit = blObjectContainer.getContainerGrossWeightUnit();
        shipmentContainer.BL_GrossVolume = blObjectContainer.getContainerGrossVolume().setScale(2, RoundingMode.HALF_UP);
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

        PartiesModel shipmentNotify = additionalDetails.getNotifyParty();

        UnlocationsResponse pol = null, pod = null, origin = null, destination = null, paidPlace = null, placeOfIssue = null, placeOfSupply = null;

        List<Object> criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getOriginPort()
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        Object unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            pol = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getDestinationPort()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            pod = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getOrigin()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            origin = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                shipment.getCarrierDetails().getDestination()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            destination = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                additionalDetails.getPaidPlace()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            paidPlace = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                additionalDetails.getPlaceOfIssue()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            placeOfIssue = unlocationsResponse.get(0);

        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                additionalDetails.getPlaceOfSupply()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            placeOfSupply = unlocationsResponse.get(0);

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
        dictionary.put(ReportConstants.ORIGIN_COUNTRY, origin != null ? origin.getCountry() : null);
        dictionary.put(ReportConstants.DESCRIPTION,shipment.getGoodsDescription());
        dictionary.put(ReportConstants.SHIPMENT_TYPE,shipment.getDirection());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, shipment.getDirection() != null ? Character.toUpperCase(shipment.getDirection().charAt(0)) : null);
        Long containerCount = 0L;
        if(shipment.getContainersList().size() > 0)
        {
            for (ContainerModel container : shipment.getContainersList())
            {
                if (container.getContainerCount() != null && container.getContainerCount() != 0)
                {
                    containerCount += container.getContainerCount();
                }
            }
        }
        dictionary.put(ReportConstants.CONTAINER_COUNT, containerCount);

        dictionary.put(ReportConstants.ETA, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEta() : null);
        dictionary.put(ReportConstants.ETD,shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEtd() : null);
        dictionary.put(ReportConstants.ATA,shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAta() : null);
        dictionary.put(ReportConstants.ATD,shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAtd() : null);
        dictionary.put(ReportConstants.DATE_OF_DEPARTURE, dictionary.get(ReportConstants.ATD) == null ? dictionary.get(ReportConstants.ETD) : dictionary.get(ReportConstants.ATD));
        dictionary.put(ReportConstants.SYSTEM_DATE, LocalDateTime.now());
        dictionary.put(ReportConstants.ONBOARD_DATE, additionalDetails.getOnBoardDate());
        dictionary.put(ReportConstants.ESTIMATED_READY_FOR_PICKUP, pickup != null ? pickup.getEstimatedPickupOrDelivery() : null);
        dictionary.put(ReportConstants.DATE_OF_ISSUE, additionalDetails.getDateOfIssue());
        dictionary.put(ReportConstants.DATE_OF_RECEIPT, additionalDetails.getDateOfReceipt());

        dictionary.put(ReportConstants.INCO_TERM, shipment.getIncoterms());
        dictionary.put(ReportConstants.CHARGEABLE, shipment.getChargable());
        dictionary.put(ReportConstants.CHARGEABLE_UNIT, shipment.getChargeableUnit());
        dictionary.put(ReportConstants.TRANSPORT_MODE, shipment.getTransportMode());
        MasterData masterData = getMasterListData(MasterDataType.TRANSPORT_MODE, shipment.getTransportMode());
        dictionary.put(ReportConstants.TRANSPORT_MODE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getTransportMode());
        masterData = getMasterListData(MasterDataType.CUSTOM_SHIPMENT_TYPE, shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_TYPE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_NUMBER, shipment.getShipmentId());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, shipment.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.ADDITIONAL_TERMS, shipment.getAdditionalTerms());

        dictionary.put(ReportConstants.PACKS,shipment.getNoOfPacks());
        dictionary.put(ReportConstants.PACKS_UNIT,shipment.getPacksUnit());
        masterData = getMasterListData(MasterDataType.PACKS_UNIT, shipment.getPacksUnit());
        dictionary.put(ReportConstants.PACKS_UNIT_DESC, masterData != null && StringUtility.isNotEmpty(masterData.getItemDescription()) ? masterData.getItemDescription() : shipment.getPacksUnit());
        dictionary.put(ReportConstants.GROSS_WEIGHT,shipment.getWeight());
        dictionary.put(ReportConstants.GROSS_WEIGHT_UNIT,shipment.getWeightUnit());
        dictionary.put(ReportConstants.GROSS_VOLUME,shipment.getVolume());
        dictionary.put(ReportConstants.GROSS_VOLUME_UNIT,shipment.getVolumeUnit());

        dictionary.put(ReportConstants.DELIVERY_CFS, delivery != null && delivery.getSourceDetail() != null ? delivery.getSourceDetail().getOrgData().get(FULL_NAME) : null);
        dictionary.put(ReportConstants.PICKUP_CFS, pickup != null && pickup.getDestinationDetail() != null ? pickup.getDestinationDetail().getOrgData().get(FULL_NAME) : null);
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
        if(placeOfIssue != null)
        {
            masterData = getMasterListData(MasterDataType.COUNTRIES, placeOfIssue.getCountry());
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
                            getValueFromMap(consignerAddress,"Zip_PostCode"));
                    dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get(COMPANY_NAME));
                    dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get("ContactPerson"));
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
                            getValueFromMap(consigneeAddress,"Zip_PostCode"));
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
                            getValueFromMap(notifyAddress,"Zip_PostCode")
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
                if(clientAddress != null)
                {
                    client = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(clientAddress, COMPANY_NAME), getValueFromMap(clientAddress, ADDRESS1),
                            getValueFromMap(clientAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(clientAddress, CITY), getValueFromMap(clientAddress, COUNTRY)),
                            getValueFromMap(clientAddress,"Email"),  getValueFromMap(clientAddress, CONTACT_PHONE),
                            getValueFromMap(clientAddress,"Zip_PostCode"));
                    dictionary.put(ReportConstants.CLIENT_NAME, getValueFromMap(clientAddress, COMPANY_NAME));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_1,getValueFromMap(clientAddress, ADDRESS1));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_PHONE,getValueFromMap(clientAddress, CONTACT_PHONE));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_MOBILE,getValueFromMap(clientAddress,"Mobile"));
                    dictionary.put(ReportConstants.CLIENT_ADDRESS_CONTACT_PERSON, getValueFromMap(clientAddress,"ContactPerson"));
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
        List<Object> criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                arrivalDetails.getLastForeignPort()
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        Object unlocations = masterDataFactory.getMasterDataService().fetchUnlocationData(commonV1ListRequest).getData();
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
        if(unlocationsResponse.size() > 0)
            lastForeignPort = unlocationsResponse.get(0);

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
                exportAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(addressData.get(COMPANY_NAME).toString(), addressData.get(ADDRESS1).toString(),
                        addressData.get(ADDRESS2).toString(),
                        ReportHelper.getCityCountry(addressData.get(CITY).toString(), addressData.get(COUNTRY).toString()),
                        null, addressData.get(CONTACT_PHONE).toString(),
                        addressData.get("Zip_PostCode").toString()
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
                importAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(addressData.get(COMPANY_NAME).toString(), addressData.get(ADDRESS1).toString(),
                        addressData.get(ADDRESS2).toString(),
                        ReportHelper.getCityCountry(addressData.get(CITY).toString(), addressData.get(COUNTRY).toString()),
                        null, addressData.get(CONTACT_PHONE).toString(),
                        addressData.get("Zip_PostCode").toString()
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
            List<String> consolNotifyPartyAddress = ReportHelper.getOrgAddressWithPhoneEmail(addressData.get(COMPANY_NAME).toString(), addressData.get(ADDRESS1).toString(),
                    addressData.get(ADDRESS2).toString(),
                    ReportHelper.getCityCountry(addressData.get(CITY).toString(), addressData.get(COUNTRY).toString()),
                    null, addressData.get(CONTACT_PHONE).toString(),
                    addressData.get("Zip_PostCode").toString()
            );
            dictionary.put(ReportConstants.CONSOL_NOTIFY_ADDRESS, consolNotifyPartyAddress);
        }
    }

    public void populateBlFields(Hbl hbl, Map<String, Object> dictionary)
    {
        if (hbl == null) return;
        HblPartyDto hblNotify = hbl.getHblNotifyParty().get(0);
        List<String> notify = ReportHelper.getOrgAddress(hblNotify.getName(), hblNotify.getAddress(), null, null, hblNotify.getEmail(), null);
        dictionary.put(ReportConstants.BL_NOTIFY_PARTY, notify);
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
        MasterData masterData = getMasterListData(MasterDataType.PACKS_UNIT, hblDataDto.getPackageType());
        dictionary.put(ReportConstants.PACKS_UNIT_DESC, masterData != null && !masterData.getItemDescription().isEmpty() ? masterData.getItemDescription() : hblDataDto.getPackageType());
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
        dictionary.put(ReportConstants.USER_EMAIL, user.Email);
        dictionary.put(ReportConstants.TENANT_CURRENCY, user.CompanyCurrency);
    }

    public MasterData getMasterListData(MasterDataType type, String ItemValue)
    {
        if (StringUtility.isEmpty(ItemValue)) return null;
        MasterListRequest masterListRequest = MasterListRequest.builder().ItemType(type.getDescription()).ItemValue(ItemValue).build();
        List<MasterListRequest> masterListRequests = new ArrayList<>();
        masterListRequests.add(masterListRequest);
        Object masterDataList = masterDataFactory.getMasterDataService().fetchMultipleMasterData(masterListRequests).getData();
        List<MasterData> masterData = jsonHelper.convertValueToList(masterDataList, MasterData.class);
        if(masterData == null || masterData.isEmpty())
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
        for(var entry : containerTypeCountMap.entrySet()) {
            ContainerCountByCode countByCode = new ContainerCountByCode();
            countByCode.ContainerTypeCode = entry.getKey();
            countByCode.ContainerCount = entry.getValue();
            containerCountByCode.add(countByCode);
        }
        return containerCountByCode;
    }

    public Awb getHawb(Long Id) {
        List<Awb> awb = awbRepository.findByShipmentId(Id);
        if(awb != null && !awb.isEmpty())
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
        String[] addressList = address.split("\r\n");
        addressList = Arrays.stream(addressList)
                .filter(Objects::nonNull)
                .map(String::trim)
                .filter(Predicate.isEqual("").negate())
                .toArray(String[]::new);
        details.addAll(Arrays.asList(addressList));
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

    public static DateTimeFormatter GetDPWDateFormatOrDefault()
    {
        return DateTimeFormatter.ofPattern("MM/dd/yyyy");
    }

    public static String ConvertToDPWDateFormat(LocalDateTime date)
    {
        String strDate = "";
        if (date != null)
        {
            strDate = date.format(GetDPWDateFormatOrDefault());
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
                Arrays.asList("LocCode"),
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
                shipmentContainer.consignerCompanyName = consigner.getAddressData().get(COMPANY_NAME).toString();
                shipmentContainer.consignerAddress1 = consigner.getAddressData().get(ADDRESS1).toString();
                shipmentContainer.consignerAddress2 = consigner.getAddressData().get(ADDRESS2).toString();
                shipmentContainer.consignerCountry = consigner.getAddressData().get(COUNTRY).toString();
                shipmentContainer.consignerZip = consigner.getAddressData().get(ZIP_POST_CODE).toString();
            }

            PartiesModel consignee = shipment.getConsignee();
            if(consignee != null && consignee.getAddressData() != null) {
                shipmentContainer.consigneeCompanyName = consignee.getAddressData().get(COMPANY_NAME).toString();
                shipmentContainer.consigneeAddress1 = consignee.getAddressData().get(ADDRESS1).toString();
                shipmentContainer.consigneeAddress2 = consignee.getAddressData().get(ADDRESS2).toString();
                shipmentContainer.consigneeCountry = consignee.getAddressData().get(COUNTRY).toString();
                shipmentContainer.consigneeZip = consignee.getAddressData().get(ZIP_POST_CODE).toString();
            }

            PartiesModel notify = shipment.getAdditionalDetails().getNotifyParty();
            if(notify != null && notify.getAddressData() != null) {
                shipmentContainer.notifyCompanyName = notify.getAddressData().get(COMPANY_NAME).toString();
                shipmentContainer.notifyAddress1 = notify.getAddressData().get(ADDRESS1).toString();
                shipmentContainer.notifyAddress2 = notify.getAddressData().get(ADDRESS2).toString();
                shipmentContainer.notifyCountry = notify.getAddressData().get(COUNTRY).toString();
                shipmentContainer.notifyZip = notify.getAddressData().get(ZIP_POST_CODE).toString();
            }

            shipmentContainer.shipmentContainers = shipment.getContainersList()
                    .stream()
                    .map(i -> getShipmentContainer(
                            jsonHelper.convertValue(i, ContainerModel.class)
                    )).toList();
            shipmentContainer.consigneeAddressFreeText = getPartyAddress(shipment.getConsignee());
            shipmentContainer.consignerAddressFreeText = getPartyAddress(shipment.getConsigner());
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
            response.ConsignerCompanyName = consigner.getAddressData().get(COMPANY_NAME).toString();
            response.ConsigneeCompanyName = consignee.getAddressData().get(COMPANY_NAME).toString();
            response.Weight = shipment.getWeight();
            response.WeightUnit = shipment.getWeightUnit();
            response.Consigner = getPartyAddress(shipment.getConsigner());
            response.Consignee =getPartyAddress(shipment.getConsignee());

            response.ConsigneeAddressFreeText = getPartyAddress(shipment.getConsigner());
            response.ConsignerAddressFreeText = getPartyAddress(shipment.getConsignee());
            response.NotifyPartyAddressFreeText = getPartyAddress(shipment.getAdditionalDetails().getNotifyParty());
//            response.Description = shipment.Description;
            response.ConsignerLocalName = consigner.getAddressData().get(LOCAL_NAME).toString();
            response.ConsigneeLocalName = consignee.getAddressData().get(LOCAL_NAME).toString();
            response.HsnNumber = shipment.getAdditionalDetails().getHsnNumber().toString();
            response.TotalPacks = getTotalPacks(shipment);

            shipmentResponses.add(response);
        }
        return shipmentResponses;
    }

    public static Long getTotalPacks(ShipmentModel shipmentDetails){
        long sum = 0L;
        for(var packs : shipmentDetails.getPackingList()) {
            sum += Long.parseLong(packs.getPacks());
        }
        return sum;
    }


    private static List<String> getPartyAddress(PartiesModel party) {
        if(party != null && party.getAddressData() != null) {
            return ReportHelper.getOrgAddress(
                    party.getAddressData().get(COMPANY_NAME).toString(),
                    party.getAddressData().get(ADDRESS1).toString(),
                    party.getAddressData().get(ADDRESS2).toString(),
                    ReportHelper.getCityCountry(
                            party.getAddressData().get(CITY).toString(),
                            party.getAddressData().get(COUNTRY).toString()
                    ),
                    party.getAddressData().get(ZIP_POST_CODE).toString(),
                    party.getAddressData().get(STATE).toString()
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
                String entityType = (shipmentModel.getDirection() == Constants.SHIPMENT_TYPE_DRT) ? Constants.DMAWB : Constants.HAWB;
                awbDao.findByShipmentId(entityId);
            }
            return false;
        } else if (shipmentModel.getDirection().equalsIgnoreCase(EXP)) {
            return true;
        }
        return false;
    }
}

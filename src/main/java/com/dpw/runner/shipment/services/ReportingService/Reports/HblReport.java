package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.concatGroupedContainerCount;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.concatGroupedFieldValues;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.concatGroupedFields;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getAddressList;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getCityCountry;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddressWithPhoneEmail;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.numberToWords;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.impl.HblService;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HblReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private ShipmentService shipmentService;
    @Autowired
    private HblService hblService;

    @Override
    public Map<String, Object> getData(Long id) {
        HblModel hblModel = (HblModel) getDocumentModel(id);
        return populateDictionary(hblModel);
    }

    public Map<String, Object> getData(Long id, String printType) {
        validatePrinting(id, printType);
        HblModel hblModel = (HblModel) getDocumentModel(id);
        return populateDictionary(hblModel);
    }

    public void validatePrinting(Long shipmentId, String printType) {
        V1TenantSettingsResponse tenantSettings;
        tenantSettings = getCurrentTenantSettings();

        if (ReportConstants.ORIGINAL.equalsIgnoreCase(printType)) {
            ShipmentDetails shipment = getShipmentDetails(shipmentId);

            if (shipment == null) {
                throw new ReportException("No shipment found with id: " + shipmentId);
            }

            if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())
                && (Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection()) || Constants.DIRECTION_CTS.equalsIgnoreCase(shipment.getDirection()))
                && Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                && ObjectUtils.isNotEmpty(shipment.getJobType()) && !Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipment.getJobType())) {
                Hbl hblObject = getHbl(shipmentId);
                shipmentService.validateHblContainerNumberCondition(shipment);
                hblService.validateHblContainerNumberCondition(hblObject);
            }
            processMissingFields(tenantSettings, shipment);
        }
    }

    private void processMissingFields(V1TenantSettingsResponse tenantSettings, ShipmentDetails shipment) {
        if (Boolean.TRUE.equals(tenantSettings.getIsModuleValidationEnabled())) {
            List<ModuleValidationFieldType> missingFields = new ArrayList<>();

            if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())
                    && (Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection()) || Constants.DIRECTION_CTS.equalsIgnoreCase(shipment.getDirection()))
                    && (Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                    || Constants.SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipment.getShipmentType()))
                    && ObjectUtils.isNotEmpty(shipment.getJobType())
                    && !Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipment.getJobType())) {

                shipmentService.validateCarrierDetails(shipment, missingFields);
                shipmentService.validateContainerDetails(shipment, missingFields);

            }

            raiseMissingFieldException(missingFields);
        }
    }

    private void raiseMissingFieldException(List<ModuleValidationFieldType> missingFields) {
        if (ObjectUtils.isNotEmpty(missingFields)) {
            String missingFieldsDescription = missingFields.stream()
                .map(ModuleValidationFieldType::getDescription)
                .collect(Collectors.joining(" | "));
            throw new ReportException(missingFieldsDescription);
        }
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HblModel hblModel = new HblModel();
        hblModel.shipment = getShipmentByQuery(id);
        hblModel.shipmentSettingsDetails = getShipmentSettings();
        hblModel.tenantSettingsResponse = getCurrentTenantSettings();
        hblModel.user = UserContext.getUser();
        if (hblModel.shipment != null && hblModel.shipment.getConsolidationList() != null && !hblModel.shipment.getConsolidationList().isEmpty()) {
            hblModel.consolidation = hblModel.shipment.getConsolidationList().get(0);
        }
        Map<String, HblContainerDto> hblContainerDtoMap = new HashMap<>();
        hblModel.blObject = getHbl(id);
        hblModel.isHbl = true;
        if(hblModel.blObject == null) {
            hblModel.blObject = new Hbl();
            hblModel.blObject.setHblData(new HblDataDto());
            hblModel.isHbl = false;
        }
        hblModel.setCommonContainers(new ArrayList<>());
        if(hblModel.blObject.getHblContainer() != null)
        {
            for(HblContainerDto hblContainerDto : hblModel.blObject.getHblContainer())
            {
                hblContainerDtoMap.put(hblContainerDto.getContainerNumber(), hblContainerDto);
            }
        }
        processShipmentContainerList(hblModel, hblContainerDtoMap);
        // UnLocations Master-data
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(hblModel.shipment);
        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = masterDataUtils.getLocationDataFromCache(new HashSet<>(unlocoRequests), EntityTransferConstants.LOCATION_SERVICE_GUID);
        for (Map.Entry<String, EntityTransferUnLocations> entry : entityTransferUnLocationsMap.entrySet()) {
            String key = entry.getKey();
            UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
            unlocationsMap.put(key, value);
        }
        // Master lists Master-data
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromShipment(hblModel.shipment);
        masterListRequest.addAll(createMasterListsRequestFromUnLocoMap(unlocationsMap));
        Map<String, EntityTransferMasterLists> entityTransferMasterListsMap = masterDataUtils.fetchMasterListFromCache(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).collect(Collectors.toList())).build());
        Map<Integer, Map<String, MasterData>> masterListsMap = new HashMap<>();
        for (Map.Entry<String, EntityTransferMasterLists> entry : entityTransferMasterListsMap.entrySet()) {
            String key = entry.getKey();
            String[] parts = key.split("#");
            String itemType = parts[0];
            String itemValue = parts[1];
            MasterDataType masterDataType = MasterDataType.valueOf(itemType);
            int masterDataKey = masterDataType.getId();
            MasterData masterData = jsonHelper.convertValue(entry.getValue(), MasterData.class);
            masterListsMap.computeIfAbsent(masterDataKey, k -> new HashMap<>()).put(itemValue, masterData);
        }
        if (masterListsMap.containsKey(MasterDataType.PAYMENT.getId()) && masterListsMap.get(MasterDataType.PAYMENT.getId()).containsKey(hblModel.shipment.getPaymentTerms()))
            hblModel.paymentTerms = masterListsMap.get(MasterDataType.PAYMENT.getId()).get(hblModel.shipment.getPaymentTerms()).getItemDescription();
        if (masterListsMap.containsKey(MasterDataType.SERVICE_MODE.getId()) && masterListsMap.get(MasterDataType.SERVICE_MODE.getId()).containsKey(hblModel.shipment.getServiceType()))
            hblModel.serviceMode = masterListsMap.get(MasterDataType.SERVICE_MODE.getId()).get(hblModel.shipment.getServiceType()).getItemDescription();

        processAdditionalDetails(hblModel, masterListsMap, unlocationsMap);

        processCarrierDetails(hblModel, unlocationsMap);

        processBookingCarriages(hblModel);
        hblModel.noofPackages = 0;
        processHblContainer(hblModel);
        hblModel.tenant = getTenant();

        return hblModel;
    }

    private void processShipmentContainerList(HblModel hblModel, Map<String, HblContainerDto> hblContainerDtoMap) {
        if(hblModel.shipment.getContainersList() != null)
        {
            for(ContainerModel container : hblModel.shipment.getContainersList())
            {
                ShipmentContainers shipmentContainer = getShipmentContainer(container);
                if(hblContainerDtoMap.containsKey(container.getContainerNumber()))
                {
                    populateBLContainer(shipmentContainer, hblContainerDtoMap.get(container.getContainerNumber()));
                }
                shipmentContainer.BL_SealNumber = container.getCustomsSealNumber();
                shipmentContainer.ShipmentPacksUnit = shipmentContainer.PacksUnitDescription;
                hblModel.getCommonContainers().add(shipmentContainer);
            }
        }
    }

    private void processAdditionalDetails(HblModel hblModel, Map<Integer, Map<String, MasterData>> masterListsMap, Map<String, UnlocationsResponse> unlocationsMap) {
        if (hblModel.shipment.getAdditionalDetails() == null)
            return;

        if (masterListsMap.containsKey(MasterDataType.RELEASE_TYPE.getId()) && masterListsMap.get(MasterDataType.RELEASE_TYPE.getId()).containsKey(hblModel.shipment.getAdditionalDetails().getReleaseType()) )
            hblModel.releaseType = masterListsMap.get(MasterDataType.RELEASE_TYPE.getId()).get(hblModel.shipment.getAdditionalDetails().getReleaseType()).getItemDescription();

        // PaidPlace master data
        if (!Objects.isNull(hblModel.shipment.getAdditionalDetails().getPaidPlace()) && unlocationsMap.containsKey(hblModel.shipment.getAdditionalDetails().getPaidPlace())) {
            UnlocationsResponse location = unlocationsMap.get(hblModel.shipment.getAdditionalDetails().getPaidPlace());
            if (masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(location.getCountry()))
                hblModel.paidPlaceCountry = masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(location.getCountry()).getItemDescription();
        }
        // IssuePlace master data
        if (!Objects.isNull(hblModel.shipment.getAdditionalDetails().getPlaceOfIssue()) && unlocationsMap.containsKey(hblModel.shipment.getAdditionalDetails().getPlaceOfIssue())) {
            hblModel.placeOfIssue = unlocationsMap.get(hblModel.shipment.getAdditionalDetails().getPlaceOfIssue());
            if (masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(hblModel.placeOfIssue.getCountry()))
                hblModel.issuePlaceCountry = masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(hblModel.placeOfIssue.getCountry()).getItemDescription();

        }
    }

    private void processCarrierDetails(HblModel hblModel, Map<String, UnlocationsResponse> unlocationsMap) {
        if (hblModel.shipment.getCarrierDetails() != null) {
            // OriginPort master data
            if (!Objects.isNull(hblModel.shipment.getCarrierDetails().getOriginPort()) && unlocationsMap.containsKey(hblModel.shipment.getCarrierDetails().getOriginPort())) {
                hblModel.polPort = unlocationsMap.get(hblModel.shipment.getCarrierDetails().getOriginPort());
                hblModel.polName = hblModel.polPort.getName();
                hblModel.polCountry =  hblModel.polPort.getCountry();
            }
            // IssuePlace master data
            if (!Objects.isNull(hblModel.shipment.getCarrierDetails().getDestinationPort()) && unlocationsMap.containsKey(hblModel.shipment.getCarrierDetails().getDestinationPort())) {
                hblModel.podPort = unlocationsMap.get(hblModel.shipment.getCarrierDetails().getDestinationPort());
                hblModel.podCountry = hblModel.podPort.getCountry();
                hblModel.podName = hblModel.podPort.getName();
            }
        }
    }

    private void processBookingCarriages(HblModel hblModel) {
        List<BookingCarriageModel> bookingCarriages = hblModel.shipment.getBookingCarriagesList();
        BookingCarriageModel bookingCarriage = null;
        if(bookingCarriages != null)
        {
            for (BookingCarriageModel carriage : bookingCarriages) {
                if (Objects.equals(carriage.getCarriageType(), "PreCarriage")) {
                    bookingCarriage = carriage;
                    break;
                }
            }
        }

        if(bookingCarriage != null)
        {
            String vessel = bookingCarriage.getVessel();
            Set<String> vesselIds = new HashSet<>();
            vesselIds.add(vessel);
            Map<String, EntityTransferVessels> entityTransferVesselsMap = masterDataUtils.getVesselDataFromCache(vesselIds);
            Map<String, VesselsResponse> vesselsResponseMap = new HashMap<>();
            for (Map.Entry<String, EntityTransferVessels> entry : entityTransferVesselsMap.entrySet()) {
                String key = entry.getKey();
                VesselsResponse value = jsonHelper.convertValue(entry.getValue(), VesselsResponse.class);
                vesselsResponseMap.put(key, value);
            }
            if(!vesselsResponseMap.isEmpty() && vesselsResponseMap.containsKey(vessel))
                hblModel.preCarriageVessel = vesselsResponseMap.get(vessel);
        }
    }

    private void processHblContainer(HblModel hblModel) {
        if(hblModel.shipment.getContainersList() != null && !hblModel.shipment.getContainersList().isEmpty()) {
            hblModel.setContainerCountGrouped(new HashMap<>());
            hblModel.setContainerWeightGrouped(new HashMap<>());
            hblModel.setContainerVolumeGrouped(new HashMap<>());
            for (ContainerModel container: hblModel.shipment.getContainersList()) {
                hblModel.noofPackages = CommonUtils.isStringNullOrEmpty(container.getPacks()) ? 0 : Long.parseLong(container.getPacks()) + hblModel.noofPackages;
                if(container.getContainerCode() != null) {
                    if(hblModel.getContainerCountGrouped().containsKey(container.getContainerCode()))
                        hblModel.getContainerCountGrouped().put(container.getContainerCode(), hblModel.getContainerCountGrouped().get(container.getContainerCode()) + container.getContainerCount());
                    else
                        hblModel.getContainerCountGrouped().put(container.getContainerCode(), container.getContainerCount());
                }
                processContainerPacks(hblModel, container);
                processContainerWeightVolume(hblModel, container);
            }
        }
    }

    private void processContainerPacks(HblModel hblModel, ContainerModel container) {
        if(container.getPacksType() != null) {
            Long packs = !CommonUtils.isStringNullOrEmpty(container.getPacks()) ? Long.parseLong(container.getPacks()) : 0;
            if(hblModel.getContainerCountGrouped().containsKey(container.getPacksType()))
                hblModel.getContainerCountGrouped().put(container.getPacksType(), hblModel.getContainerCountGrouped().get(container.getPacksType()) + packs);
            else
                hblModel.getContainerCountGrouped().put(container.getPacksType(), packs);
        }
    }

    private void processContainerWeightVolume(HblModel hblModel, ContainerModel container) {
        if(container.getGrossWeightUnit() != null) {
            double grossWeight = 0;
            if(container.getGrossWeight() != null)
                grossWeight = container.getGrossWeight().doubleValue();
            hblModel.getContainerWeightGrouped().put(container.getGrossWeightUnit(), hblModel.getContainerWeightGrouped().containsKey(container.getGrossWeightUnit()) ? hblModel.getContainerWeightGrouped().get(container.getGrossWeightUnit()) + grossWeight : grossWeight);
        }
        if(container.getGrossVolumeUnit() != null) {
            double grossVolume = 0;
            if(container.getGrossVolume() != null)
                grossVolume = container.getGrossVolume().doubleValue();
            hblModel.getContainerVolumeGrouped().put(container.getGrossVolumeUnit(), hblModel.getContainerWeightGrouped().containsKey(container.getGrossVolumeUnit()) ? hblModel.getContainerWeightGrouped().get(container.getGrossVolumeUnit()) + grossVolume : grossVolume);
        }
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        HblModel hblModel = (HblModel) documentModel;
        validateAirAndOceanDGCheck(hblModel.shipment);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(hblModel.shipment, getDPWDateFormatOrDefault(v1TenantSettingsResponse));
        setBlObject(hblModel);
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        hblModel.shipment.setTransportInstructionId(hblModel.getTransportInstructionId());
        populateShipmentFields(hblModel.shipment, dictionary);
        populateConsolidationFields(hblModel.consolidation, dictionary);
        jsonDateFormat(dictionary);
        processBlObject(hblModel, dictionary);

        dictionary.put(SHIPMENT_DETAIL_DATE_OF_ISSUE_IN_CAPS, StringUtility.toUpperCase(convertToDPWDateFormat(LocalDateTime.now(), "ddMMMy", true)));
        dictionary.put(ReportConstants.NO_OF_PACKAGES1, hblModel.noofPackages);
        dictionary.put(ReportConstants.CONTAINER_COUNT_GROUPED, concatGroupedContainerCount(hblModel.getContainerCountGrouped()));
        dictionary.put(ReportConstants.CONTAINER_PACKS_GROUPED, concatGroupedContainerCount(hblModel.getContainerPacksGrouped()));
        processDecimalPlacesTag(hblModel, dictionary, v1TenantSettingsResponse);
        dictionary.put(ReportConstants.DELIVERY_AGENT, null);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(hblModel.getCommonContainers()));
        processHblShipmentTag(hblModel, dictionary);
        processAdditionalDetails(hblModel, dictionary);
        processReferenceNumber(hblModel, dictionary);

        populateBillChargesFields(hblModel.shipment, dictionary);
        processReferenceNumbersList(hblModel, dictionary);
        addCargoLocationTag(hblModel, dictionary);
        processNotifyParty(hblModel, dictionary);
        processConsignerConsignee(hblModel, dictionary);

        List<String> deliveryParty = getOrgAddress(hblModel.shipment.getAdditionalDetails().getNotifyParty());
        if (hblModel.blObject.getHblNotifyParty() != null && !hblModel.blObject.getHblNotifyParty().isEmpty())
            dictionary.put(NOTIFY_PARTY_ADDRESS, getAddressList(hblModel.blObject.getHblNotifyParty().get(0).getAddress()));
        addDescriptionTags(hblModel, dictionary);

        dictionary.put(NOTIFY_PARTY_FREETEXT, dictionary.get(NOTIFY_PARTY));
        dictionary.put(CONSIGNEE_FREETEXT, dictionary.get(CONSIGNEE));
        dictionary.put(CONSIGNER_FREETEXT, dictionary.get(CONSIGNER));

        dictionary.put(ORIGINAL_OR_COPY, ORIGINAL);
        processHblTenant(hblModel, dictionary);
        dictionary.put(BL_VESSEL_NAME, StringUtility.toUpperCase(hblModel.blObject.getHblData().getVesselName()));
        dictionary.put(BL_VOYAGE, StringUtility.toUpperCase(hblModel.blObject.getHblData().getVoyage()));

        // SHIPMENT FIELDS
        dictionary.put(ENTRY_REF_NUMBER, hblModel.shipment.getEntryRefNo());
        addVesselNameTag(hblModel, dictionary);
        dictionary.put(TRANSPORT_MODE, hblModel.shipment.getTransportMode());
        dictionary.put(CHARGEABLE, convertToWeightNumberFormat(hblModel.shipment.getChargable(), v1TenantSettingsResponse));
        dictionary.put(CHARGEABLE_UNIT, hblModel.shipment.getChargeableUnit());
        dictionary.put(FREIGHT_OVERSEAS, AmountNumberFormatter.format(hblModel.shipment.getFreightOverseas(), hblModel.shipment.getFreightOverseasCurrency(), hblModel.tenantSettingsResponse));
        dictionary.put(FREIGHT_OVERSEAS_CURRENCY, hblModel.shipment.getFreightOverseasCurrency());
        dictionary.put(DESCRIPTION, hblModel.blObject != null ? StringUtility.toUpperCase(hblModel.blObject.getHblData().getCargoDescription())
                : StringUtility.toUpperCase(hblModel.shipment.getGoodsDescription()));
        dictionary.put(ORIGINALS, hblModel.shipment.getAdditionalDetails().getOriginal() == null ? 1 : hblModel.shipment.getAdditionalDetails().getOriginal());
        dictionary.put(ORIGINAL_WORDS, numberToWords(hblModel.shipment.getAdditionalDetails().getOriginal() == null ? 1 : hblModel.shipment.getAdditionalDetails().getOriginal()));
        dictionary.put(ISSUE_PLACE_NAME, hblModel.placeOfIssue != null ? StringUtility.toUpperCase(hblModel.placeOfIssue.getName()) : "");
        dictionary.put(ISSUE_PLACE_COUNTRY, hblModel.placeOfIssue != null ? StringUtility.toUpperCase(hblModel.placeOfIssue.getCountry()) : "");
        dictionary.put(ISSUEPLACECOUNTRYNAME, hblModel.issuePlaceCountry); //MasterData
        dictionary.put(BL_COMMENTS, hblModel.blObject.getHblData().getBlComments());
        dictionary.put(MARKS_AND_NUMBER, hblModel.blObject.getHblData().getMarksAndNumbers());
        if (!Objects.isNull(hblModel.blObject.getHblData().getMarksAndNumbers()))
            dictionary.put(MARKS_N_NUMS_CAPS, hblModel.blObject.getHblData().getMarksAndNumbers().toUpperCase());

        processCommonContainers(hblModel, v1TenantSettingsResponse, dictionary);
        dictionary.put(IS_IMPORT, hblModel.shipment.getDirection().equals(IMP));

        dictionary.put(DELIVERY_PARTY, deliveryParty);
        dictionary.put(LOGO, getLogoPath(hblModel.user));

        addContainerCountTag(hblModel, dictionary);
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();

        dictionary.put(CURRENT_DATE, convertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(HOUSE_BILL, StringUtility.toUpperCase(hblModel.shipment.getHouseBill()));
        dictionary.put(SHIPMENT_ID, hblModel.shipment.getShipmentId());
        dictionary.put(REFERENCE_NO, hblModel.shipment.getBookingReference());
        dictionary.put(SERVICE_MODE_DESCRIPTION, hblModel.serviceMode); //MasterListData
        dictionary.put(PAYMENT_TERMS, StringUtility.toUpperCase(hblModel.paymentTerms)); //MasterListData
        dictionary.put(RELEASE_TYPE, hblModel.releaseType); //MasterListData


        addCarrierDetailsTag(hblModel, dictionary, tsDateTimeFormat, v1TenantSettingsResponse);

        addActualDeliveryTag(hblModel, dictionary);

        processShipmentPickupDetails(hblModel, dictionary, tsDateTimeFormat);
        dictionary.put(PLACE_OF_DELIVERY, hblModel.podCountry);
        processHblData(hblModel, dictionary, v1TenantSettingsResponse);
        getPartiesModel(hblModel, dictionary);

        dictionary.put(USER_FULLNAME, hblModel.user.getDisplayName());
        dictionary.put(USER_NAME, hblModel.user.getUsername());
        dictionary.put(USER_EMAIL, hblModel.user.getEmail());

        processOnBoard(hblModel, dictionary, tsDateTimeFormat);


        dictionary.put(ATTENTION, dictionary.get(CONSIGNEE));
        dictionary.put(DO_NO, hblModel.shipment.getShipmentId());
        if (!Objects.isNull(hblModel.shipment.getConsignee()) && !Objects.isNull(hblModel.shipment.getConsignee().getAddressData()))
            dictionary.put(MESSERS, getValueFromMap(hblModel.shipment.getConsignee().getAddressData(), FULL_NAME));
        dictionary.put(IGM_NO, hblModel.shipment.getAdditionalDetails().getIGMFileNo());
        dictionary.put(MBL_NUMBER, hblModel.shipment.getMasterBill());
        dictionary.put(HBL_NUMBER, hblModel.shipment.getHouseBill());

        addWeightVolumeTags(hblModel, dictionary);

        dictionary.put(DESC_OF_GOODS, hblModel.shipment.getGoodsDescription());
        dictionary.put(JOB_NO, hblModel.shipment.getShipmentId());
        processPolPodTags(dictionary, hblModel);
        dictionary.put(TRANSPORT_MODE, hblModel.shipment.getTransportMode());

        addConsolidationTags(hblModel, dictionary);

        addStatusTag(hblModel, dictionary);

        processPackingList(hblModel, dictionary, v1TenantSettingsResponse);

        dictionary.put(PICKUP_ORDER_CONTACT_PERSON, EMPTY_STRING);

        dictionary.put(CONTAINER_SUMMARY, hblModel.shipment.getSummary());
        dictionary.put(SUMMARY, hblModel.shipment.getSummary());

        dictionary.put(BOOKING_NUMBER, hblModel.shipment.getBookingNumber());
        dictionary.put(ADDITIONAL_TERMS, hblModel.shipment.getAdditionalTerms());

        dictionary.put(UCR_REFERENCE, EMPTY_STRING);
        dictionary.put(EMPTY_TRUCK_IN_DATE, EMPTY_STRING);
        dictionary.put(LOADED_TRUCK_GATE_OUT_DATE, EMPTY_STRING);
        addPickUpDetailsTag(hblModel, dictionary, tsDateTimeFormat, v1TenantSettingsResponse);

        processPreCarriage(hblModel, dictionary);

        // ====================  END OF MIGRATION PLACEHOLDER ===================
        dictionary.put(ReportConstants.PAID_PLACE_COUNTRY_NAME, StringUtility.toUpperCase(hblModel.paidPlaceCountry));
        dictionary.put(ReportConstants.SERVICE_MODE_DESCRIPTION, hblModel.serviceMode);
        dictionary.put(ReportConstants.PPCC, hblModel.paymentTerms);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(hblModel.getCommonContainers()));

        dictionary.put(ReportConstants.PRE_CARRIAGE, hblModel.preCarriageVessel != null ? hblModel.preCarriageVessel.getName() : null);
        addPickUpAndDeliveryDetailsTag(hblModel, dictionary);

        addPackTags(hblModel, dictionary, v1TenantSettingsResponse);

        dictionary.put(MARKS_N_NUMS, StringUtility.toUpperCase(hblModel.shipment.getMarksNum()));

        addPickupFromTag(hblModel, dictionary);

        addDeliverToTag(hblModel, dictionary);

        if (hblModel.consolidation != null) {
            this.populateConsolidationReportData(dictionary, null, hblModel.consolidation.getId());
        }

        if (hblModel.shipment != null) {
            this.populateShipmentReportData(dictionary, null, hblModel.shipment.getId());
            this.getContainerDetails(hblModel.getShipment(), dictionary);
            this.getPackingDetails(hblModel.getShipment(), dictionary);
        }

        return dictionary;
    }

    private void processAdditionalDetails(HblModel hblModel, Map<String, Object> dictionary) {
        if (hblModel.consolidation != null) {
            processHblConsolidation(hblModel, dictionary);
        } else if (!Objects.isNull(hblModel.shipment) && !Objects.isNull(hblModel.shipment.getAdditionalDetails())) {
            processShipmentAdditionalDetails(hblModel, dictionary);
        }
    }

    private void setBlObject(HblModel hblModel) {
        if(hblModel.blObject == null) {
            hblModel.blObject = new Hbl();
            hblModel.blObject.setHblData(new HblDataDto());
        }
    }

    private void processBlObject(HblModel hblModel, Map<String, Object> dictionary) {
        if (hblModel.blObject != null) {
            String blObjectJson = jsonHelper.convertToJson(hblModel.blObject);
            Map<String, Object> blObjectDictionary = jsonHelper.convertJsonToMap(blObjectJson);
            jsonDateFormat(blObjectDictionary);
            for (Map.Entry<String, Object> entry : blObjectDictionary.entrySet()) {
                String key = entry.getKey();
                Object value = entry.getValue();
                dictionary.remove(key);
                dictionary.put(key, value);
            }
        }
    }

    private void processDecimalPlacesTag(HblModel hblModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        Integer decimalPlaces = hblModel.shipmentSettingsDetails == null || hblModel.shipmentSettingsDetails.getDecimalPlaces() == null ? 2 : hblModel.shipmentSettingsDetails.getDecimalPlaces();
        dictionary.put(ReportConstants.CONTAINER_WEIGHT_WITH_XSEPARATED, concatGroupedFieldValues(hblModel.getContainerWeightGrouped(), decimalPlaces));
        dictionary.put(ReportConstants.CONTAINER_VOLUME_WITH_XSEPARATED, concatGroupedFieldValues(hblModel.getContainerVolumeGrouped(), decimalPlaces));
        dictionary.put(ReportConstants.CONTAINER_WEIGHT_GROUPED, concatGroupedFields(hblModel.getContainerWeightGrouped(), decimalPlaces));
        dictionary.put(ReportConstants.CONTAINER_VOLUME_GROUPED, concatGroupedFields(hblModel.getContainerVolumeGrouped(), decimalPlaces));
        processWeightVolumeChargable(hblModel, decimalPlaces, v1TenantSettingsResponse, dictionary);
    }

    private void addDescriptionTags(HblModel hblModel, Map<String, Object> dictionary) {
        String description = hblModel.blObject.getHblData().getCargoDescription();
        description = description != null ? description : hblModel.shipment.getGoodsDescription();
        if (!Objects.isNull(description))
            dictionary.put(DESCRIPTION_CAPS, description.toUpperCase());
        dictionary.put(DESCRIPTION_ORIGINAL, getAddressList(description));
    }

    private void processConsignerConsignee(HblModel hblModel, Map<String, Object> dictionary) {
        List<String> consigner = null;
        List<String> consignee = null;
        if (hblModel.blObject != null && hblModel.isHbl && !hblModel.shipment.getTransportMode().equals(AIR)) {
            List<String> notify = getNotifyOrgAddress(hblModel.blObject, hblModel.shipmentSettingsDetails);
            if (!Objects.isNull(notify)) {
                dictionary.put(BL_NOTIFY_PARTY, notify);
                dictionary.put(BL_NOTIFY_PARTY_CAPS, notify.stream().map(String::toUpperCase).toList());
            }
            if(Boolean.TRUE.equals(hblModel.shipmentSettingsDetails.getDisableBlPartiesName())) {
                consigner = getOrgAddress(null, hblModel.blObject.getHblData().getConsignorAddress(),
                        null, null, null, null);
                consignee = getOrgAddress(null, hblModel.blObject.getHblData().getConsigneeAddress(),
                        null, null, null, null);
            } else {
                consigner = getOrgAddress(hblModel.blObject.getHblData().getConsignorName(), hblModel.blObject.getHblData().getConsignorAddress(),
                        null, null, null, null);
                consignee = getOrgAddress(hblModel.blObject.getHblData().getConsigneeName(), hblModel.blObject.getHblData().getConsigneeAddress(),
                        null, null, null, null);
            }
        } else {
            consigner = getShipmentConsigner(hblModel, consigner, dictionary);
            consignee = getShipmentConsignee(hblModel, consignee, dictionary);
        }

        dictionary.put(CONSIGNER, consigner);
        dictionary.put(CONSIGNER_CAPS, consigner != null ? consigner.stream().map(String::toUpperCase).toList() : null);
        dictionary.put(CONSIGNER_ADDRESS, getAddressList(hblModel.blObject.getHblData().getConsignorAddress()));
        dictionary.put(CONSIGNEE, consignee);
        if(!Objects.isNull(consignee))
            dictionary.put(CONSIGNEE_CAPS, consignee.stream().map(String::toUpperCase).collect(Collectors.toList()));
    }

    private void addActualDeliveryTag(HblModel hblModel, Map<String, Object> dictionary) {
        if (hblModel.shipment.getDeliveryDetails() != null && hblModel.shipment.getDeliveryDetails().getActualPickupOrDelivery() != null)
            dictionary.put(ACTUAL_DELIVERY, DateTimeFormatter.ofPattern(ReportConstants.DPW_DATE_FORMAT_OR_DEFAULT_STRING + " hh:mm ss")
                    .format(hblModel.shipment.getDeliveryDetails().getActualPickupOrDelivery()));
    }

    private void processPolPodTags(Map<String, Object> dictionary, HblModel hblModel) {
        dictionary.put(PORT_OF_LOADING, hblModel.polPort != null ? hblModel.polPort.getPortName() : null);
        dictionary.put(PORT_OF_LOADING_COUNTRY, hblModel.polPort != null ? hblModel.polPort.getCountry() : null);
        dictionary.put(PORT_OF_DISCHARGE, hblModel.podPort != null ? hblModel.podPort.getPortName() : null);
        dictionary.put(PORT_OF_DISCHARGE_COUNTRY, hblModel.podPort != null ? hblModel.podPort.getCountry() : null);
        dictionary.put(PORT_OF_FINAL_DESTINATION, hblModel.podPort != null ? hblModel.podPort.getPortName() : null);
        dictionary.put(PORT_OF_FINAL_DESTINATION_COUNTRY, hblModel.podPort != null ? hblModel.podPort.getCountry() : null);
    }

    private void addConsolidationTags(HblModel hblModel, Map<String, Object> dictionary) {
        ConsolidationModel consolidation = getFirstConsolidationFromShipmentId(hblModel.shipment.getId());
        if(consolidation != null && consolidation.getPayment() != null)
            dictionary.put(PPCC, consolidation.getPayment());
    }

    private void addStatusTag(HblModel hblModel, Map<String, Object> dictionary) {
        if(hblModel.shipment.getPickupDetails() != null && hblModel.shipment.getPickupDetails().getActualPickupOrDelivery() != null)
            dictionary.put(STATUS, CONFIRMED);
        else
            dictionary.put(STATUS, PLANNED);
    }

    private void addCarrierDetailsTag(HblModel hblModel, Map<String, Object> dictionary, String tsDateTimeFormat, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (hblModel.shipment.getCarrierDetails().getEtd() != null)
            dictionary.put(ETD, convertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getEtd(), tsDateTimeFormat, v1TenantSettingsResponse));
        if (hblModel.shipment.getCarrierDetails().getEta() != null)
            dictionary.put(ETA, convertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getEta(), tsDateTimeFormat, v1TenantSettingsResponse));
        if (hblModel.shipment.getAdditionalDetails().getDateOfIssue() != null) {
            dictionary.put(DATE_OF_ISSUE_MDY, convertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getDateOfIssue(), tsDateTimeFormat, true));
            dictionary.put(DATE_OF_ISSUE_DMY, convertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getDateOfIssue(), "dd/MM/yyyy", true));
            dictionary.put(DATE_OF_ISSUE_DMMY, convertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getDateOfIssue(), "dd-MMM-yyyy", true));
        }
        if (hblModel.shipment.getAdditionalDetails().getDateOfReceipt() != null)
            dictionary.put(DATE_OF_RECEIPT, convertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getDateOfReceipt(), tsDateTimeFormat, v1TenantSettingsResponse));
        if (hblModel.shipment.getCarrierDetails().getAtd() != null) {
            LocalDateTime atd = hblModel.shipment.getCarrierDetails().getAtd();
            dictionary.put(ATD_MDY, convertToDPWDateFormat(atd, tsDateTimeFormat, v1TenantSettingsResponse));
            dictionary.put(ATD_DMY, DateTimeFormatter.ofPattern("dd/MM/yyyy").format(atd));
            dictionary.put(ATD_DMMY, DateTimeFormatter.ofPattern("dd-MMM-yyyy").format(atd));
        }
        dictionary.put(FLIGHT_CARRIER, hblModel.shipment.getCarrierDetails().getShippingLine());
        dictionary.put(FLIGHT_NUMBER, hblModel.shipment.getCarrierDetails().getFlightNumber());
        if (hblModel.shipment.getCarrierDetails().getAtd() != null)
            dictionary.put(ATD, convertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getAtd(), tsDateTimeFormat, v1TenantSettingsResponse));
        if (hblModel.shipment.getCarrierDetails().getAta() != null)
            dictionary.put(ATA, convertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getAta(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(FLIGHT_NAME, hblModel.shipment.getCarrierDetails().getShippingLine());
        if(hblModel.shipment.getCarrierDetails()!= null && hblModel.shipment.getCarrierDetails().getOrigin() != null) {
            dictionary.put(PLACE_OF_RECEIPT_ALIAS, hblModel.shipment.getCarrierDetails().getOrigin());
            dictionary.put(PLACE_OF_RECIEPT_IN_CAPS, hblModel.shipment.getCarrierDetails().getOrigin().toUpperCase());
        }
        assert hblModel.shipment.getCarrierDetails() != null;
        dictionary.put(PLACE_OF_DELIVERY_ALIAS, hblModel.shipment.getCarrierDetails().getDestination());
        dictionary.put(VOYAGE, hblModel.shipment.getCarrierDetails().getVoyage());
        dictionary.put(VESSEL_BERTHING_DATE, convertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getVesselBerthingDate(), tsDateTimeFormat, v1TenantSettingsResponse));
    }

    private void addVesselNameTag(HblModel hblModel, Map<String, Object> dictionary) {
        String shipmentCarrierVesselId = hblModel.shipment.getCarrierDetails().getVessel();
        Set<String> vesselIds = new HashSet<>();
        vesselIds.add(shipmentCarrierVesselId);
        Map<String, EntityTransferVessels> entityTransferVesselsMap = masterDataUtils.getVesselDataFromCache(vesselIds);
        Map<String, VesselsResponse> vesselsResponseMap = new HashMap<>();
        for (Map.Entry<String, EntityTransferVessels> entry : entityTransferVesselsMap.entrySet()) {
            String key = entry.getKey();
            VesselsResponse value = jsonHelper.convertValue(entry.getValue(), VesselsResponse.class);
            vesselsResponseMap.put(key, value);
        }
        VesselsResponse vesselsResponse = vesselsResponseMap.get(shipmentCarrierVesselId);
        if(vesselsResponse != null)
            dictionary.put(VESSEL_NAME, vesselsResponse.getName());
    }

    private void addCargoLocationTag(HblModel hblModel, Map<String, Object> dictionary) {
        if(hblModel.shipment.getShipmentAddresses() != null){
            dictionary.put(CARGO_LOCATION, hblModel.shipment.getShipmentAddresses().stream()
                .filter(i -> i.getType().equalsIgnoreCase(CAL))
                .findFirst()
                .map(ReportHelper::getOrgAddressDetails)
                .orElse(null));
        }
    }

    private void processHblShipmentTag(HblModel hblModel, Map<String, Object> dictionary) {
        if(hblModel.shipment == null)
            return;
        if (hblModel.shipment.getFreightLocal() != null)
            dictionary.put(ReportConstants.FREIGHT_LOCAL, hblModel.shipment.getFreightLocal());
        if (hblModel.shipment.getFreightLocalCurrency() != null && !hblModel.shipment.getFreightLocalCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_LOCAL_CURRENCY, hblModel.shipment.getFreightLocalCurrency());
        if (hblModel.shipment.getFreightOverseas() != null)
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, AmountNumberFormatter.format(hblModel.shipment.getFreightOverseas(), hblModel.shipment.getFreightOverseasCurrency(), hblModel.tenantSettingsResponse));
        if (hblModel.shipment.getFreightOverseasCurrency() != null && !hblModel.shipment.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, hblModel.shipment.getFreightOverseasCurrency());
        if (hblModel.shipment.getShipmentAddresses() != null && !hblModel.shipment.getShipmentAddresses().isEmpty()) {
            for (PartiesModel shipmentAddress : hblModel.shipment.getShipmentAddresses()) {
                if (shipmentAddress.getType() != null && shipmentAddress.getType().equalsIgnoreCase(CUSTOM_HOUSE_AGENT) && shipmentAddress.getOrgData() != null && getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME) != null) {
                    dictionary.put(CHA_PARTY_DESCRIPTION, getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME));
                }
            }
        }
    }

    private void addPickUpDetailsTag(HblModel hblModel, Map<String, Object> dictionary, String tsDateTimeFormat, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (hblModel.shipment.getPickupDetails() != null)
        {
            PickupDeliveryDetailsModel pickupDetails = hblModel.shipment.getPickupDetails();
            dictionary.put(UCR_REFERENCE, pickupDetails.getUcrReference());
            dictionary.put(EMPTY_TRUCK_IN_DATE, convertToDPWDateFormat(pickupDetails.getEmptyTruckInDate(), tsDateTimeFormat, v1TenantSettingsResponse));
            dictionary.put(LOADED_TRUCK_GATE_OUT_DATE, convertToDPWDateFormat(pickupDetails.getLoadedTruckGateOutDate(), tsDateTimeFormat, v1TenantSettingsResponse));
            dictionary.put(PICKUP_PORT_TRANSPORT_ADVISED, convertToDPWDateFormat(pickupDetails.getPortTransportAdvised(), tsDateTimeFormat, v1TenantSettingsResponse));
        }
    }

    private void addPickUpAndDeliveryDetailsTag(HblModel hblModel, Map<String, Object> dictionary) {
        PickupDeliveryDetailsModel pickup = hblModel.shipment.getPickupDetails();
        if(pickup != null && pickup.getTransporterDetail() != null && pickup.getTransporterDetail().getAddressData() != null)
        {
            Map<String, Object> address = pickup.getTransporterDetail().getAddressData();
            dictionary.put(ReportConstants.PICKUP_TRANSPORT, ReportHelper.getOrgAddressWithPhoneEmail(
                    StringUtility.convertToString(address.get(COMPANY_NAME)), StringUtility.convertToString(address.get(ReportConstants.ADDRESS1)), StringUtility.convertToString(address.get(ReportConstants.ADDRESS2)),
                    StringUtility.convertToString(address.get(ReportConstants.COUNTRY)), StringUtility.convertToString(address.get(ReportConstants.EMAIL)), StringUtility.convertToString(address.get(ReportConstants.CONTACT_PHONE)),
                    null
            ));
        }
        PickupDeliveryDetailsModel delivery = hblModel.shipment.getDeliveryDetails();
        if (delivery != null && delivery.getAgentDetail() != null && delivery.getAgentDetail().getAddressData() != null) {
            Map<String, Object> address = delivery.getAgentDetail().getAddressData();
            dictionary.put(ReportConstants.DELIVERY_AGENT, ReportHelper.getOrgAddressWithPhoneEmail(
                    StringUtility.convertToString(address.get(COMPANY_NAME)), StringUtility.convertToString(address.get(ReportConstants.ADDRESS1)), StringUtility.convertToString(address.get(ReportConstants.ADDRESS2)),
                    StringUtility.convertToString(address.get(ReportConstants.COUNTRY)), StringUtility.convertToString(address.get(ReportConstants.EMAIL)), StringUtility.convertToString(address.get(ReportConstants.CONTACT_PHONE)),
                    null
            ));
        }
    }

    private void addPackTags(HblModel hblModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(Boolean.TRUE.equals(hblModel.isHbl)) {
            if(hblModel.blObject.getHblData().getPackageCount() != null)
                dictionary.put(PACKS, StringUtility.toUpperCase(getDPWWeightVolumeFormat(BigDecimal.valueOf(hblModel.blObject.getHblData().getPackageCount()), 0, v1TenantSettingsResponse)));
            dictionary.put(PACKS_UNIT, StringUtility.toUpperCase(Constants.MPK.equals(hblModel.blObject.getHblData().getPackageType()) ? StringUtility.toUpperCase(Constants.PACKAGES) : StringUtility.toUpperCase(hblModel.blObject.getHblData().getPackageType())));
            dictionary.put(PACKS_UNIT_DESC, Constants.MULTI_PACK.equals(masterListDescriptionPacksUnit(hblModel.blObject.getHblData().getPackageType())) ? StringUtility.toUpperCase(Constants.PACKAGES) : StringUtility.toUpperCase(masterListDescriptionPacksUnit(hblModel.blObject.getHblData().getPackageType())));
            dictionary.put(ReportConstants.DESCRIPTION, StringUtility.toUpperCase(hblModel.blObject.getHblData().getCargoDescription()));
        } else {
            if(hblModel.shipment.getNoOfPacks() != null)
                dictionary.put(PACKS, StringUtility.toUpperCase(StringUtility.toUpperCase(getDPWWeightVolumeFormat(BigDecimal.valueOf(hblModel.shipment.getNoOfPacks()), 0, v1TenantSettingsResponse))));
            dictionary.put(PACKS_UNIT, Constants.MPK.equals(hblModel.shipment.getPacksUnit()) ? Constants.PACKAGES : hblModel.shipment.getPacksUnit());
            dictionary.put(PACKS_UNIT_DESC, Constants.MULTI_PACK.equals(masterListDescriptionPacksUnit(hblModel.shipment.getPacksUnit())) ? StringUtility.toUpperCase(Constants.PACKAGES) : StringUtility.toUpperCase(masterListDescriptionPacksUnit(hblModel.shipment.getPacksUnit())));
            dictionary.put(DESCRIPTION, StringUtility.toUpperCase(hblModel.shipment.getGoodsDescription()));
        }
    }

    private void addWeightVolumeTags(HblModel hblModel, Map<String, Object> dictionary) {
        if(hblModel.shipment.getVolumetricWeight() != null)
            dictionary.put(V_WEIGHT_AND_UNIT_AIR, String.format(REGEX_S_S, ReportHelper.twoDecimalPlacesFormat(
                    hblModel.shipment.getVolumetricWeight().toString()), hblModel.shipment.getVolumetricWeightUnit()));
        if(hblModel.shipment.getWeight() != null)
            dictionary.put(WEIGHT_AND_UNIT_AIR, String.format(REGEX_S_S, ReportHelper.twoDecimalPlacesFormat(
                    hblModel.shipment.getWeight().toString()), hblModel.shipment.getWeightUnit()));
        if(hblModel.shipment.getVolume() != null)
            dictionary.put(VOLUME_AND_UNIT_AIR, String.format(REGEX_S_S, ReportHelper.twoDecimalPlacesFormat(
                    hblModel.shipment.getVolume().toString()), hblModel.shipment.getVolumeUnit()));
    }

    private List<String> getShipmentConsigner(HblModel hblModel, List<String> consigner, Map<String, Object> dictionary) {
        PartiesModel shipmentConsigner = hblModel.shipment.getConsigner();
        if (shipmentConsigner != null) {
            Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
            if (consignerAddress != null) {
                consigner = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consignerAddress, COMPANY_NAME), getValueFromMap(consignerAddress, ADDRESS1),
                        getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                        getValueFromMap(consignerAddress, EMAIL), getValueFromMap(consignerAddress, CONTACT_PHONE),
                        getValueFromMap(consignerAddress, "Zip_PostCode"));
                dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get(COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get(CONTACT_PERSON));
            }
        }
        return consigner;
    }

    private List<String> getShipmentConsignee(HblModel hblModel, List<String> consignee, Map<String, Object> dictionary) {
        PartiesModel shipmentConsignee = hblModel.shipment.getConsignee();
        if (shipmentConsignee != null) {
            Map<String, Object> consigneeAddress = shipmentConsignee.getAddressData();
            if (consigneeAddress != null) {
                consignee = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consigneeAddress, COMPANY_NAME), getValueFromMap(consigneeAddress, ADDRESS1),
                        getValueFromMap(consigneeAddress, ADDRESS2),
                        ReportHelper.getCityCountry(getValueFromMap(consigneeAddress, CITY), getValueFromMap(consigneeAddress, COUNTRY)),
                        getValueFromMap(consigneeAddress, EMAIL), getValueFromMap(consigneeAddress, CONTACT_PHONE),
                        getValueFromMap(consigneeAddress, "Zip_PostCode"));
                dictionary.put(ReportConstants.CONSIGNEE_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNEE_CONTACT_PERSON, getValueFromMap(consigneeAddress, CONTACT_PERSON));
            }
        }
        return consignee;
    }

    private void addDeliverToTag(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel deliveryTo = null;

        if(hblModel.shipment.getDeliveryDetails() != null)
            deliveryTo = hblModel.shipment.getDeliveryDetails().getDestinationDetail();
        if (deliveryTo != null && deliveryTo.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryTo.getAddressData();
            populateAddress(addressMap, dictionary, ReportConstants.DELIVERY_TO);
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put(ReportConstants.DELIVERY_TO, address);
        }
    }

    private void addPickupFromTag(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel pickupFrom = null;
        if(hblModel.shipment.getPickupDetails() != null)
            pickupFrom = hblModel.shipment.getPickupDetails().getSourceDetail();
        if (pickupFrom != null && pickupFrom.getAddressData() != null)
        {
            Map<String, Object> addressMap = pickupFrom.getAddressData();
            populateAddress(addressMap, dictionary, ReportConstants.PICKUP_FROM);
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put(ReportConstants.PICKUP_FROM, address);
        }
    }

    private void processOnBoard(HblModel hblModel, Map<String, Object> dictionary, String tsDateTimeFormat) {
        String onBoard = hblModel.shipment.getAdditionalDetails().getOnBoard();
        if( onBoard != null && !onBoard.isEmpty()){
            String onBoardValue = null;
            if(onBoard.equalsIgnoreCase("SHP")) {
                onBoardValue = "Shipped On Board";
            }
            if(onBoard.equalsIgnoreCase("RFS")) {
                onBoardValue = "Received For Shipment";
            }
            dictionary.put(ONBOARD_DATE, onBoardValue);
            dictionary.put(ONBOARD_TYPE_DATE, hblModel.shipment.getAdditionalDetails().getOnBoardDate() != null ?
                    StringUtility.toUpperCase(convertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getOnBoardDate(), tsDateTimeFormat, true)) : null);
        }
    }

    private void processReferenceNumbersList(HblModel hblModel, Map<String, Object> dictionary) {
        if (hblModel.shipment.getReferenceNumbersList() != null) {
            dictionary.put(AMS_NUMBER, hblModel.shipment.getReferenceNumbersList().stream()
                .filter(i -> i.getType().equalsIgnoreCase(AMS))
                .findFirst()
                .map(ReferenceNumbersModel::getReferenceNumber)
                .orElse(null));
        }
    }

    private void processHblData(HblModel hblModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (hblModel.blObject != null && hblModel.blObject.getHblData() != null) {
            dictionary.put(BL_PLACE_OF_DELIVERY, StringUtility.toUpperCase(hblModel.blObject.getHblData().getPlaceOfDelivery()));
            dictionary.put(BL_WEIGHT, convertToWeightNumberFormat(hblModel.blObject.getHblData().getCargoGrossWeight(), v1TenantSettingsResponse));
            dictionary.put(BL_WEIGHT_UNIT, hblModel.blObject.getHblData().getCargoGrossWeightUnit());
            dictionary.put(BL_NETWEIGHT, convertToWeightNumberFormat(hblModel.blObject.getHblData().getCargoNetWeight(), v1TenantSettingsResponse));
            dictionary.put(BL_NETWEIGHT_UNIT, hblModel.blObject.getHblData().getCargoNetWeightUnit());
            dictionary.put(BL_DELIVERYAGENT, StringUtility.toUpperCase(hblModel.blObject.getHblData().getDeliveryAgent()));
            dictionary.put(BL_DELIVERYAGENT_ADDRESS, StringUtility.toUpperCase(hblModel.blObject.getHblData().getDeliveryAgentAddress()));
            dictionary.put(BL_CARGO_TERMS_DESCRIPTION, StringUtility.toUpperCase(hblModel.blObject.getHblData().getCargoTermsDescription()));
            dictionary.put(BL_REMARKS_DESCRIPTION, StringUtility.toUpperCase(hblModel.blObject.getHblData().getBlRemarksDescription()));
            dictionary.put(ReportConstants.BL_PLACE_OF_RECEIPT, StringUtility.toUpperCase(hblModel.blObject.getHblData().getPlaceOfReceipt()));
            dictionary.put(ReportConstants.BL_PORT_OF_LOADING, hblModel.blObject.getHblData().getPortOfLoad() == null ? "" : StringUtility.toUpperCase(hblModel.blObject.getHblData().getPortOfLoad()));
            dictionary.put(ReportConstants.BL_PORT_OF_DISCHARGE, hblModel.blObject.getHblData().getPortOfDischarge() == null ? "" : StringUtility.toUpperCase(hblModel.blObject.getHblData().getPortOfDischarge()));
        }
    }

    private void processPreCarriage(HblModel hblModel, Map<String, Object> dictionary) {
        List<String> bookingPreCarriageMode = new ArrayList<>();
        List<String> bookingCarriageVesselVoyage = new ArrayList<>();

        if(hblModel.shipment.getBookingCarriagesList() != null){
            for(var bookingCarriage : hblModel.shipment.getBookingCarriagesList()) {
                processPreCarriageBooking(bookingCarriage, bookingPreCarriageMode, bookingCarriageVesselVoyage);
            }
        }
        bookingCarriageVesselVoyage.replaceAll(StringUtility::toUpperCase);

        if (!bookingPreCarriageMode.isEmpty())
            dictionary.put(PRE_CARRIAGE_MODE, String.join(",", bookingPreCarriageMode));
        if (!bookingCarriageVesselVoyage.isEmpty())
            dictionary.put(PRE_CARRIAGE_VESSEL_VOYAGE, String.join(",", bookingCarriageVesselVoyage));
    }

    private void processPreCarriageBooking(BookingCarriageModel bookingCarriage, List<String> bookingPreCarriageMode, List<String> bookingCarriageVesselVoyage) {
        if(bookingCarriage.getCarriageType().equals(PRE_CARRIAGE)){
            var carriage = getMasterListData(MasterDataType.CARRIAGE_MODE , bookingCarriage.getCarriageMode());
            if(!Objects.isNull(carriage))
                bookingPreCarriageMode.add(carriage.getItemDescription());
            var vessel = getVesselsData(bookingCarriage.getVessel());
            if(!Objects.isNull(vessel))
                bookingCarriageVesselVoyage.add(vessel.getName());
            bookingCarriageVesselVoyage.add(bookingCarriage.getVoyage());
        }
    }

    private void processPackingList(HblModel hblModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(!Objects.isNull(hblModel.shipment.getPackingList()) && !hblModel.shipment.getPackingList().isEmpty()) {
            getPackingDetails(hblModel.shipment, dictionary);
            dictionary.put(HAS_PACK_DETAILS, true);
            var hazardousCheck = hblModel.shipment.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getHazardous()) && x.getHazardous());
            var temperatureCheck = hblModel.shipment.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getIsTemperatureControlled()) && x.getIsTemperatureControlled());
            dictionary.put(HAS_DANGEROUS_GOODS, hazardousCheck);
            dictionary.put(HAS_TEMPERATURE_DETAILS, temperatureCheck);

            List<Map<String, Object>> valuesContainer = getValuesContainer(hblModel, v1TenantSettingsResponse);
            dictionary.put(PACKING_LIST, valuesContainer);
        } else {
            dictionary.put(HAS_PACK_DETAILS, false);
        }
    }

    private List<Map<String, Object>> getValuesContainer(HblModel hblModel, V1TenantSettingsResponse v1TenantSettingsResponse) {
        List<Map<String, Object>> valuesContainer = new ArrayList<>();
        for (PackingModel packingModel : hblModel.shipment.getPackingList()) {
            Map<String, Object> packContJson = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(packingModel));
            if (packContJson.containsKey(PACKS) && packContJson.get(PACKS) != null)
                packContJson.put(PACKS, StringUtility.toUpperCase(getDPWWeightVolumeFormat(new BigDecimal(StringUtility.convertToString(packContJson.get(PACKS))), 0, v1TenantSettingsResponse)));
            if (packContJson.containsKey(LENGTH) && packContJson.get(LENGTH) != null)
                packContJson.put(LENGTH, getDPWWeightVolumeFormat(new BigDecimal(StringUtility.convertToString(packContJson.get(LENGTH))), 0, v1TenantSettingsResponse));
            if (packContJson.containsKey(WIDTH) && packContJson.get(WIDTH) != null)
                packContJson.put(WIDTH, getDPWWeightVolumeFormat(new BigDecimal(StringUtility.convertToString(packContJson.get(WIDTH))), 0, v1TenantSettingsResponse));
            if (packContJson.containsKey(HEIGHT) && packContJson.get(HEIGHT) != null)
                packContJson.put(HEIGHT, getDPWWeightVolumeFormat(new BigDecimal(StringUtility.convertToString(packContJson.get(HEIGHT))), 0, v1TenantSettingsResponse));
            valuesContainer.add(packContJson);
        }
        return valuesContainer;
    }

    private void getPartiesModel(HblModel hblModel, Map<String, Object> dictionary) {
        addDeliveryToTag(hblModel, dictionary);

        addDeliveryAgentTag(hblModel, dictionary);

        addDeliveryTransportTag(hblModel, dictionary);

        addDeliveryCfsTag(hblModel, dictionary);

        addPickUpTransportCompany(hblModel, dictionary);


        PartiesModel pickupCfs = null;
        if(hblModel.shipment.getPickupDetails() != null)
            pickupCfs = hblModel.shipment.getPickupDetails().getDestinationDetail();
        if (pickupCfs != null && pickupCfs.getAddressData() != null)
        {
            Map<String, Object> addressMap = pickupCfs.getAddressData();
            populateAddress(pickupCfs.getAddressData(), dictionary, "PickupCfs");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("PickupCfs", address);
        }

        PartiesModel client = hblModel.shipment.getClient();
        if(client != null && client.getAddressData() != null) {
            Map<String, Object> addressMap = client.getAddressData();
            List<String> clientAddress = getOrgAddress(getValueFromMap(addressMap, COMPANY_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    null, null);
            if(getValueFromMap(addressMap, FULL_NAME) != null) {
                clientAddress.add(0, getValueFromMap(addressMap, FULL_NAME));
            }
            dictionary.put(CLIENT_ADRS, clientAddress);
        }
    }

    private void addPickUpTransportCompany(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel pickupTransportCompany = null;
        if(hblModel.shipment.getPickupDetails() != null)
            pickupTransportCompany = hblModel.shipment.getPickupDetails().getTransporterDetail();
        if (pickupTransportCompany != null && pickupTransportCompany.getAddressData() != null)
        {
            Map<String, Object> addressMap = pickupTransportCompany.getAddressData();
            populateAddress(addressMap, dictionary, "PickupTransport");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("PickupTransport", address);
        }

        if (pickupTransportCompany != null && pickupTransportCompany.getAddressData() != null && getValueFromMap(pickupTransportCompany.getAddressData(), CONTACT_PERSON) != null) {
                dictionary.put(PICKUP_ORDER_CONTACT_PERSON, getValueFromMap(pickupTransportCompany.getAddressData(), CONTACT_PERSON));
        }
    }

    private void addDeliveryCfsTag(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel deliveryCfs = null;
        if(hblModel.shipment.getDeliveryDetails() != null)
            deliveryCfs = hblModel.shipment.getDeliveryDetails().getSourceDetail();
        if (deliveryCfs != null && deliveryCfs.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryCfs.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryCfs");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryCfs", address);
        }
    }

    private void addDeliveryTransportTag(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel deliveryTransportCompany = null;
        if(hblModel.shipment.getDeliveryDetails() != null)
            deliveryTransportCompany = hblModel.shipment.getDeliveryDetails().getTransporterDetail();
        if (deliveryTransportCompany != null && deliveryTransportCompany.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryTransportCompany.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryTransport");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryTransport", address);
        }
    }

    private void addDeliveryAgentTag(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel deliveryAgentAddress = null;
        if(hblModel.shipment.getDeliveryDetails() != null)
            deliveryAgentAddress = hblModel.shipment.getDeliveryDetails().getAgentDetail();
        if (deliveryAgentAddress != null && deliveryAgentAddress.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryAgentAddress.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryAgent");
            var deliveryAgent = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryAgent", deliveryAgent);
        }
    }

    private void addDeliveryToTag(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel deliveryToAddress = null;
        if (hblModel.shipment.getDeliveryDetails() != null)
            deliveryToAddress = hblModel.shipment.getDeliveryDetails().getDestinationDetail();
        if (deliveryToAddress != null && deliveryToAddress.getAddressData() != null) {
            Map<String, Object> addressMap = deliveryToAddress.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryTo");
            var deliveryTo = getOrgAddress(getValueFromMap(addressMap, COMPANY_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryTo", deliveryTo);
        }
    }

    private void processWeightVolumeChargable(HblModel hblModel, Integer decimalPlaces, V1TenantSettingsResponse v1TenantSettingsResponse, Map<String, Object> dictionary) {
        if (!Objects.isNull(hblModel.shipment.getWeight())) {
            BigDecimal weight = hblModel.shipment.getWeight().setScale(decimalPlaces, RoundingMode.HALF_UP);
            String weightString = convertToWeightNumberFormat(weight, v1TenantSettingsResponse);
            dictionary.put(WEIGHT, StringUtility.toUpperCase(weightString));
            dictionary.put(WEIGHT_AND_UNIT, String.format(REGEX_S_S, weightString, hblModel.shipment.getWeightUnit()));
        }
        if (!Objects.isNull(hblModel.shipment.getVolume())) {
            BigDecimal volume = hblModel.shipment.getVolume().setScale(decimalPlaces, RoundingMode.HALF_UP);
            String volumeString = convertToVolumeNumberFormat(volume, v1TenantSettingsResponse);
            dictionary.put(VOLUME, StringUtility.toUpperCase(volumeString));
            dictionary.put(VOLUME_AND_UNIT, String.format(REGEX_S_S, volumeString, hblModel.shipment.getVolumeUnit()));
        }
        if (!Objects.isNull(hblModel.shipment.getChargable())) {
            BigDecimal chargeable = hblModel.shipment.getChargable().setScale(decimalPlaces, RoundingMode.HALF_UP);
            String chargeableString = convertToWeightNumberFormat(chargeable, v1TenantSettingsResponse);
            dictionary.put(CHARGEABLE, chargeableString);
            dictionary.put(CHARGABLE_AND_UNIT, String.format(REGEX_S_S, chargeableString, hblModel.shipment.getChargeableUnit()));
            dictionary.put(CHARGEABLE_AND_UNIT, dictionary.get(CHARGABLE_AND_UNIT));
        }
    }

    private void addContainerCountTag(HblModel hblModel, Map<String, Object> dictionary) {
        if (!Objects.isNull(hblModel.shipment.getShipmentContainersList())) {
            int containerCount = 0;
            for (var container : hblModel.shipment.getShipmentContainersList()) {
                containerCount += container.getContainerCount();
            }
            dictionary.put(CONTAINER_COUNT, StringUtility.toUpperCase(numberToWords(containerCount)));
        }
    }

    private void processHblTenant(HblModel hblModel, Map<String, Object> dictionary) {
        if (!Objects.isNull(hblModel.tenant)) {
            dictionary.put(TENANT_NAME, StringUtility.toUpperCase(hblModel.tenant.tenantName));
            dictionary.put(TENANT_ADDRESS_1, hblModel.tenant.address1);
            dictionary.put(TENANT_ADDRESS_2, hblModel.tenant.address2);
            dictionary.put(TENANT_EMAIL, hblModel.tenant.email);
            dictionary.put(TENANT_FAX, hblModel.tenant.fax);
            dictionary.put(TENANT_GSTIN, hblModel.tenant.vatRegNumber);
            dictionary.put(TENANT_PAN_NUMBER, hblModel.tenant.panNumber);
            dictionary.put(TENANT_CITY, hblModel.tenant.city);
            dictionary.put(TENANT_STATE, hblModel.tenant.state);
            dictionary.put(TENANT_COUNTRY, hblModel.tenant.country);
            dictionary.put(TENANT_COUNTRY_PHONE, hblModel.tenant.phone);
            dictionary.put(TENANT_MOBILE, hblModel.tenant.mobile);
            dictionary.put(TENANT_ZIP_POST_CODE, hblModel.tenant.zipPostCode);
            dictionary.put(TENANT_URL, hblModel.tenant.websiteUrl);
            dictionary.put(TENANT_COMPANY_REG_NUMBER, hblModel.tenant.companyRegNumber);
            dictionary.put(TENANT, ReportHelper.getListOfStrings(hblModel.tenant.tenantName, hblModel.tenant.address1,
                    hblModel.tenant.address2, hblModel.tenant.city, hblModel.tenant.state, hblModel.tenant.zipPostCode,
                    hblModel.tenant.country, hblModel.tenant.email, hblModel.tenant.websiteUrl, hblModel.tenant.phone));
        }
    }

    private void processNotifyParty(HblModel hblModel, Map<String, Object> dictionary) {
        if (!Objects.isNull(hblModel.shipment) && !Objects.isNull(hblModel.shipment.getAdditionalDetails()) && !Objects.isNull(hblModel.shipment.getAdditionalDetails().getNotifyParty())) {
            PartiesModel notifyParty = hblModel.shipment.getAdditionalDetails().getNotifyParty();
            List<String> notifyPartyAddress = getOrgAddressWithPhoneEmail(notifyParty);
            if (!Objects.isNull(notifyParty.getAddressData()) && notifyParty.getAddressData().get(FULL_NAME) != null) {
                notifyPartyAddress.add(0, getValueFromMap(notifyParty.getAddressData(), FULL_NAME));
            }
            List<String> notifyPartyDetails = ReportHelper.getOrgAddressDetails(notifyParty);
            dictionary.put(NOTIFY_PARTY_ADDRESS, notifyPartyAddress);
            dictionary.put(DELIVERY_PHONE, getValueFromMap(notifyParty.getAddressData(), MOBILE));
            dictionary.put(DELIVERY_FAX, getValueFromMap(notifyParty.getAddressData(), FAX));

            dictionary.put(NOTIFY_PARTY, notifyPartyDetails);
            dictionary.put(NOTIFY_PARTY_CAPS, notifyPartyAddress.stream().map(String::toUpperCase).toList());
        }
    }

    private void processReferenceNumber(HblModel hblModel, Map<String, Object> dictionary) {
        Optional<ReferenceNumbersModel> referenceNumber = Optional.empty();

        if (hblModel.shipment.getReferenceNumbersList() != null) {
            referenceNumber = hblModel.shipment.getReferenceNumbersList().stream().findFirst()
                    .filter(i -> i.getType().equals(ERN));
        }
        if (referenceNumber.isEmpty() && hblModel.consolidation != null && hblModel.consolidation.getReferenceNumbersList() != null) {
            referenceNumber = hblModel.consolidation.getReferenceNumbersList().stream().findFirst()
                    .filter(i -> i.getType().equals(ERN));
        }
        referenceNumber.ifPresent(i -> dictionary.put(EXPORT_REFERENCE_NUMBER, StringUtility.toUpperCase(i.getReferenceNumber())));

        if (hblModel.shipment.getReferenceNumbersList() != null) {
            referenceNumber = hblModel.shipment.getReferenceNumbersList().stream().findFirst()
                    .filter(i -> i.getType().equals(SHIPMENT_CAN_DOCUMENT));
        }
        referenceNumber.ifPresent(i -> dictionary.put(CAN_NUMBER, i.getReferenceNumber()));
    }

    private void processShipmentPickupDetails(HblModel hblModel, Map<String, Object> dictionary, String tsDateTimeFormat) {
        if(hblModel.shipment.getPickupDetails() != null){
            dictionary.put(SHIPPER_REF_NO, hblModel.shipment.getPickupDetails().getShipperRef());
            dictionary.put(PICKUP_SHIPPERS_REF, hblModel.shipment.getPickupDetails().getShipperRef());
            dictionary.put(PICKUP_INSTRUCTION, hblModel.shipment.getPickupDetails().getPickupDeliveryInstruction());
            dictionary.put(SHIPMENT_PICKUP_PICKUPINSTRUCTION, hblModel.shipment.getPickupDetails().getPickupDeliveryInstruction());
            dictionary.put(ESTIMATED_READY_FOR_PICKUP, convertToDPWDateFormatWithTime(hblModel.shipment.getPickupDetails().getEstimatedPickupOrDelivery(), tsDateTimeFormat, true));
            dictionary.put(PICKUP_TIME, dictionary.get(ESTIMATED_READY_FOR_PICKUP));
            if (hblModel.shipment.getPickupDetails().getActualPickupOrDelivery() != null) {
                dictionary.put(ReportConstants.STATUS, "Confirmed");
                dictionary.put(ReportConstants.PICKUP_TIME, convertToDPWDateFormatWithTime(hblModel.shipment.getPickupDetails().getActualPickupOrDelivery(), tsDateTimeFormat, true));
                dictionary.put(ReportConstants.PICKUPTIME_TYPE,  "Actual Pickup");
            } else {
                dictionary.put(ReportConstants.STATUS, "Planned");
                if (hblModel.shipment.getPickupDetails().getEstimatedPickupOrDelivery() != null) {
                    dictionary.put(ReportConstants.PICKUP_TIME, convertToDPWDateFormatWithTime(hblModel.shipment.getPickupDetails().getEstimatedPickupOrDelivery(), tsDateTimeFormat, true));
                } else {
                    dictionary.put(ReportConstants.PICKUP_TIME, "");
                }
                dictionary.put(ReportConstants.PICKUPTIME_TYPE, "Estimated Pickup");
            }
        }
    }

    private void processCommonContainers(HblModel hblModel, V1TenantSettingsResponse v1TenantSettingsResponse, Map<String, Object> dictionary) {
        if(hblModel.getCommonContainers() != null && !hblModel.getCommonContainers().isEmpty()) {
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ShipmentContainers shipmentContainers : hblModel.getCommonContainers()) {
                convertShipmentContainersToUpperCase(shipmentContainers);
                String shipContJsonString = jsonHelper.convertToJson(shipmentContainers);
                Map<String, Object> shipContJson = getShipContJson(v1TenantSettingsResponse, shipContJsonString);
                valuesContainer.add(shipContJson);
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
            dictionary.put(QUOTE_HAS_CONTAINERS, true);
        }
        else
            dictionary.put(QUOTE_HAS_CONTAINERS, false);
    }

    private Map<String, Object> getShipContJson(V1TenantSettingsResponse v1TenantSettingsResponse, String shipContJsonString) {
        Map<String, Object> shipContJson = jsonHelper.convertJsonToMap(shipContJsonString);
        if(shipContJson.containsKey(ReportConstants.GROSS_VOLUME) && shipContJson.get(ReportConstants.GROSS_VOLUME) != null)
            shipContJson.put(ReportConstants.GROSS_VOLUME, convertToVolumeNumberFormat(shipContJson.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
        if (shipContJson.containsKey(ReportConstants.GROSS_WEIGHT) && shipContJson.get(ReportConstants.GROSS_WEIGHT) != null)
            shipContJson.put(ReportConstants.GROSS_WEIGHT, convertToWeightNumberFormat(shipContJson.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
        if (shipContJson.containsKey(ReportConstants.NET_WEIGHT) && shipContJson.get(ReportConstants.NET_WEIGHT) != null)
            shipContJson.put(ReportConstants.NET_WEIGHT, convertToWeightNumberFormat(new BigDecimal(shipContJson.get(ReportConstants.NET_WEIGHT).toString())));
        if (shipContJson.containsKey(NO_OF_PACKAGES) && shipContJson.get(NO_OF_PACKAGES) != null) {
            shipContJson.put(NO_OF_PACKAGES, getDPWWeightVolumeFormat(new BigDecimal(StringUtility.convertToString(shipContJson.get(NO_OF_PACKAGES))), 0, v1TenantSettingsResponse));
        }
        return shipContJson;
    }

    private void processShipmentAdditionalDetails(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel receivingAgentParty = hblModel.shipment.getAdditionalDetails().getReceivingAgent();
        if (!Objects.isNull(receivingAgentParty)) {
            List<String> receivingAgent = ReportHelper.getOrgAddress(
                    getValueFromMap(receivingAgentParty.getAddressData(), COMPANY_NAME),
                    getValueFromMap(receivingAgentParty.getAddressData(), ADDRESS1),
                    getValueFromMap(receivingAgentParty.getAddressData(), ADDRESS2),
                    ReportHelper.getCityCountry(
                            getValueFromMap(receivingAgentParty.getAddressData(), CITY),
                            getValueFromMap(receivingAgentParty.getAddressData(), COUNTRY)
                    ),
                    getValueFromMap(receivingAgentParty.getAddressData(), ZIP_POST_CODE),
                    getValueFromMap(receivingAgentParty.getAddressData(), STATE)
            );
            dictionary.put(RECEIVING_AGENT_ADDRESS, receivingAgent);
        }
        PartiesModel sendingAgentParty = hblModel.shipment.getAdditionalDetails().getSendingAgent();
        if (!Objects.isNull(sendingAgentParty)) {
            List<String> sendingAgent = ReportHelper.getOrgAddress(
                    getValueFromMap(sendingAgentParty.getAddressData(), COMPANY_NAME),
                    getValueFromMap(sendingAgentParty.getAddressData(), ADDRESS1),
                    getValueFromMap(sendingAgentParty.getAddressData(), ADDRESS2),
                    ReportHelper.getCityCountry(
                            getValueFromMap(sendingAgentParty.getAddressData(), CITY),
                            getValueFromMap(sendingAgentParty.getAddressData(), COUNTRY)
                    ),
                    getValueFromMap(sendingAgentParty.getAddressData(), ZIP_POST_CODE),
                    getValueFromMap(sendingAgentParty.getAddressData(), STATE)
            );
            dictionary.put(SENDING_AGENT_ADDRESS, sendingAgent);
        }
    }

    private void processHblConsolidation(HblModel hblModel, Map<String, Object> dictionary) {
        PartiesModel receivingAgentParty = hblModel.consolidation.getReceivingAgent();
        if (!Objects.isNull(receivingAgentParty)) {
            List<String> receivingAgent = ReportHelper.getOrgAddress(
                    getValueFromMap(receivingAgentParty.getAddressData(), COMPANY_NAME),
                    getValueFromMap(receivingAgentParty.getAddressData(), ADDRESS1),
                    getValueFromMap(receivingAgentParty.getAddressData(), ADDRESS2),
                    ReportHelper.getCityCountry(
                            getValueFromMap(receivingAgentParty.getAddressData(), CITY),
                            getValueFromMap(receivingAgentParty.getAddressData(), COUNTRY)
                    ),
                    getValueFromMap(receivingAgentParty.getAddressData(), ZIP_POST_CODE),
                    getValueFromMap(receivingAgentParty.getAddressData(), STATE)
            );
            dictionary.put(RECEIVING_AGENT_NAME, getValueFromMap(receivingAgentParty.getAddressData(), COMPANY_NAME));
            dictionary.put(RECEIVING_AGENT_ADDRESS, receivingAgent);
            dictionary.put(DELIVERY_AGENT, receivingAgent);
        }
        PartiesModel sendingAgentParty = hblModel.consolidation.getSendingAgent();
        if (!Objects.isNull(sendingAgentParty)) {
            List<String> sendingAgent = ReportHelper.getOrgAddress(
                    getValueFromMap(sendingAgentParty.getAddressData(), COMPANY_NAME),
                    getValueFromMap(sendingAgentParty.getAddressData(), ADDRESS1),
                    getValueFromMap(sendingAgentParty.getAddressData(), ADDRESS2),
                    ReportHelper.getCityCountry(
                            getValueFromMap(sendingAgentParty.getAddressData(), CITY),
                            getValueFromMap(sendingAgentParty.getAddressData(), COUNTRY)
                    ),
                    getValueFromMap(sendingAgentParty.getAddressData(), ZIP_POST_CODE),
                    getValueFromMap(sendingAgentParty.getAddressData(), STATE)
            );
            dictionary.put(SENDING_AGENT_NAME, getValueFromMap(sendingAgentParty.getAddressData(), COMPANY_NAME));
            dictionary.put(SENDING_AGENT_ADDRESS, sendingAgent);
        }

        dictionary.put(AGENT_REFERENCE, hblModel.consolidation.getAgentReference());
        dictionary.put(CONSOL_NUMBER, hblModel.consolidation.getConsolidationNumber());

        dictionary.put(SENDING_AGENT_ADDRESS_FREE_TEXT, dictionary.get(SENDING_AGENT_ADDRESS));
        dictionary.put(RECEIVING_AGENT_ADDRESS_FREE_TEXT, dictionary.get(RECEIVING_AGENT_ADDRESS));
    }

    private void convertShipmentContainersToUpperCase(ShipmentContainers shipmentContainers) {
        if (shipmentContainers.ContainerNumber != null) shipmentContainers.ContainerNumber = shipmentContainers.ContainerNumber.toUpperCase();
        if (shipmentContainers.ShipmentPacksUnit != null) shipmentContainers.ShipmentPacksUnit = shipmentContainers.ShipmentPacksUnit.toUpperCase();
        if (shipmentContainers.GrossWeightUnit != null) shipmentContainers.GrossWeightUnit = shipmentContainers.GrossWeightUnit.toUpperCase();
        if (shipmentContainers.GrossVolumeUnit != null) shipmentContainers.GrossVolumeUnit = shipmentContainers.GrossVolumeUnit.toUpperCase();
        if (shipmentContainers.ContainerTypeCode != null) shipmentContainers.ContainerTypeCode = shipmentContainers.ContainerTypeCode.toUpperCase();
        if (shipmentContainers.DescriptionOfGoods != null) shipmentContainers.DescriptionOfGoods = shipmentContainers.DescriptionOfGoods.toUpperCase();
        if (shipmentContainers.CarrierSealNumber != null) shipmentContainers.CarrierSealNumber = shipmentContainers.CarrierSealNumber.toUpperCase();
        if (shipmentContainers.CustomsSealNumber != null) shipmentContainers.CustomsSealNumber = shipmentContainers.CustomsSealNumber.toUpperCase();
        if (shipmentContainers.ShipperSealNumber != null) shipmentContainers.ShipperSealNumber = shipmentContainers.ShipperSealNumber.toUpperCase();
        if (shipmentContainers.ShipmentMarksnNums != null) shipmentContainers.ShipmentMarksnNums = shipmentContainers.ShipmentMarksnNums.toUpperCase();
    }

    // isActive Criteria not clear from v1 impl
    private String masterListDescriptionPacksUnit(String packageType) {
        if (packageType == null || packageType.isEmpty())
            return packageType;

        MasterData masterData = getMasterListData(MasterDataType.PACKS_UNIT, packageType);
        return (masterData != null ? masterData.getItemDescription() : null);
    }

    private String getLogoPath(UsersDto user) {
        String basePath = "Upload/";
        var path = basePath + user.TenantId + "/Assets/" + user.HouseBillLogo;
        if (user.HouseBillLogo != null && !user.HouseBillLogo.isEmpty()) {
            return path;
        }
        return null;
    }

    private List<String> getNotifyOrgAddress(Hbl hbl, ShipmentSettingsDetails shipmentSettingsDetails)
    {
        if(hbl != null && hbl.getHblNotifyParty() != null && !hbl.getHblNotifyParty().isEmpty()) {
            HblPartyDto row = hbl.getHblNotifyParty().get(0);
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName()))
                return getOrgAddress(null, row.getAddress(), null, null, row.getEmail(), null);
            else
                return getOrgAddress(row.getName(), row.getAddress(), null, null, row.getEmail(), null);
        }
        return null;
    }
}

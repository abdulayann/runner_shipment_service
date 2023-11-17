package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.MathContext;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;

@Component
public class HblReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

    @Override
    public Map<String, Object> getData(Long id) {
        HblModel hblModel = (HblModel) getDocumentModel(id);
        return populateDictionary(hblModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HblModel hblModel = new HblModel();
        hblModel.shipment = getShipment(id);
        hblModel.shipmentSettingsDetails = getShipmentSettings(TenantContext.getCurrentTenant());
        hblModel.tenantSettingsResponse = getTenantSettings();
        hblModel.user = UserContext.getUser();
        if(hblModel.shipment != null && hblModel.shipment.getConsolidationList() != null && !hblModel.shipment.getConsolidationList().isEmpty())
        {
            hblModel.consolidation = hblModel.shipment.getConsolidationList().get(0);
        }
        Map<String, HblContainerDto> hblContainerDtoMap = new HashMap<>();
        hblModel.blObject = getHbl(id);
        hblModel.commonContainers = new ArrayList<>();
        if(hblModel.blObject.getHblContainer() != null)
        {
            for(HblContainerDto hblContainerDto : hblModel.blObject.getHblContainer())
            {
                hblContainerDtoMap.put(hblContainerDto.getContainerNumber(), hblContainerDto);
            }
        }
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
                hblModel.commonContainers.add(shipmentContainer);
            }
        }
        MasterData masterData = getMasterListData(MasterDataType.PAYMENT, hblModel.shipment.getPaymentTerms());
        hblModel.paymentTerms = (masterData != null ? masterData.getItemDescription() : null);
        masterData = getMasterListData(MasterDataType.SERVICE_MODE, hblModel.shipment.getServiceType());
        hblModel.serviceMode = (masterData != null ? masterData.getItemDescription() : null);
        masterData = getMasterListData(MasterDataType.RELEASE_TYPE, hblModel.shipment.getAdditionalDetails().getReleaseType());
        hblModel.releaseType = (masterData != null ? masterData.getItemDescription() : null);

        UnlocationsResponse paidPlace = null;
        List<Object> criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                hblModel.shipment.getAdditionalDetails().getPaidPlace()
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse != null && unlocationsResponse.size() > 0)
        {
            paidPlace = unlocationsResponse.get(0);
            masterData = getMasterListData(MasterDataType.COUNTRIES, paidPlace.getCountry());
        }
        hblModel.paidPlaceCountry = (masterData != null ? masterData.getItemDescription() : null);

        // IssuePlace master data
        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                hblModel.shipment.getAdditionalDetails().getPlaceOfIssue()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse != null && unlocationsResponse.size() > 0)
            hblModel.placeOfIssue = unlocationsResponse.get(0);
        masterData = getMasterListData(MasterDataType.COUNTRIES, Objects.isNull(hblModel.placeOfIssue) ? StringUtility.getEmptyString() : hblModel.placeOfIssue.getCountry());
        hblModel.issuePlaceCountry = (masterData != null ? masterData.getItemDescription() : null);

        // polPort
        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                hblModel.shipment.getCarrierDetails().getOriginPort()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse != null && unlocationsResponse.size() > 0)
            hblModel.polPort = unlocationsResponse.get(0);


        // podPort
        criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                hblModel.shipment.getCarrierDetails().getDestinationPort()
        );
        commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse != null && unlocationsResponse.size() > 0)
            hblModel.podPort = unlocationsResponse.get(0);


        List<BookingCarriageModel> bookingCarriages = hblModel.shipment.getBookingCarriagesList();
        BookingCarriageModel bookingCarriage = null;
        if(bookingCarriages != null)
        {
            for(int i=0; i<bookingCarriages.size(); i++)
            {
                if(Objects.equals(bookingCarriages.get(i).getCarriageType(), "PreCarriage"))
                {
                    bookingCarriage = bookingCarriages.get(i);
                    break;
                }
            }
        }
        if(bookingCarriage != null)
        {
            String vessel = bookingCarriage.getVessel();
            List<Object> vesselCriteria = Arrays.asList(
                    Arrays.asList("Mmsi"),
                    "=",
                    vessel
            );
            CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(vesselCriteria).build();
            V1DataResponse vesselResponse = v1Service.fetchVesselData(vesselRequest);
            List<VesselsResponse> vesselsResponse = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);
            if(vesselsResponse != null && vesselsResponse.size() > 0)
                hblModel.preCarriageVessel = vesselsResponse.get(0);
        }
        hblModel.noofPackages = 0;
        if(hblModel.shipment.getContainersList() != null && hblModel.shipment.getContainersList().size() > 0) {
            for (ContainerModel container: hblModel.shipment.getContainersList()) {
                hblModel.noofPackages = container.getNoOfPackages() != null ? container.getNoOfPackages() : 0 + hblModel.noofPackages;
                hblModel.containerCountGrouped = new HashMap<>();
                hblModel.containerWeightGrouped = new HashMap<>();
                hblModel.containerVolumeGrouped = new HashMap<>();
                if(container.getContainerCode() != null) {
                    if(hblModel.containerCountGrouped.containsKey(container.getContainerCode()))
                        hblModel.containerCountGrouped.put(container.getContainerCode(), hblModel.containerCountGrouped.get(container.getContainerCode()) + container.getContainerCount());
                    else
                        hblModel.containerCountGrouped.put(container.getContainerCode(), container.getContainerCount());
                }
                if(container.getPacksType() != null) {
                    if(hblModel.containerCountGrouped.containsKey(container.getPacksType()))
                        hblModel.containerCountGrouped.put(container.getPacksType(), hblModel.containerCountGrouped.get(container.getPacksType()) + Long.valueOf(container.getPacks()));
                    else
                        hblModel.containerCountGrouped.put(container.getPacksType(), Long.valueOf(container.getPacks()));
                }
                if(container.getGrossWeightUnit() != null) {
                    hblModel.containerWeightGrouped.put(container.getGrossWeightUnit(), hblModel.containerWeightGrouped.containsKey(container.getGrossWeightUnit()) ? hblModel.containerWeightGrouped.get(container.getGrossWeightUnit()) + container.getGrossWeight().longValue() : container.getGrossWeight().longValue());
                }
                if(container.getGrossVolumeUnit() != null) {
                    hblModel.containerVolumeGrouped.put(container.getGrossVolumeUnit(), hblModel.containerWeightGrouped.containsKey(container.getGrossVolumeUnit()) ? hblModel.containerWeightGrouped.get(container.getGrossVolumeUnit()) + container.getGrossVolume().longValue() : container.getGrossVolume().longValue());
                }
            }
        }
        Map<String, UnlocationsResponse> unlocationMap = getLocationData(Set.of(hblModel.shipment.getCarrierDetails().getDestinationPort()));
        UnlocationsResponse location = unlocationMap.get(hblModel.shipment.getCarrierDetails().getDestinationPort());
        if(location != null) {
            hblModel.podCountry = location.getCountry();
        }

        return hblModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        HblModel hblModel = (HblModel) documentModel;
        String json = jsonHelper.convertToJson(hblModel.shipment);
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        JsonDateFormat(dictionary);
        if(hblModel.blObject != null) {
            String blObjectJson = jsonHelper.convertToJson(hblModel.blObject);
            Map<String, Object> blObjectDictionary = jsonHelper.convertJsonToMap(blObjectJson);
            JsonDateFormat(blObjectDictionary);
            for (Map.Entry<String, Object> entry : blObjectDictionary.entrySet()) {
                String key = entry.getKey();
                Object value = entry.getValue();
                if(dictionary.containsKey(key))
                    dictionary.remove(key);
                dictionary.put(key, value);
            }
        }
        dictionary.put(ReportConstants.NoOfPackages, hblModel.noofPackages);
        dictionary.put(ReportConstants.CONTAINER_COUNT_GROUPED, concatGroupedContainerCount(hblModel.containerCountGrouped));
        dictionary.put(ReportConstants.CONTAINER_PACKS_GROUPED, concatGroupedContainerCount(hblModel.containerPacksGrouped));
        Integer decimalPlaces = hblModel.shipmentSettingsDetails.getDecimalPlaces() == null ? 2 : hblModel.shipmentSettingsDetails.getDecimalPlaces();
        dictionary.put(ReportConstants.ContainerWeightWithXSeparated, concatGroupedFieldValues(hblModel.containerWeightGrouped, decimalPlaces));
        dictionary.put(ReportConstants.ContainerVolumeWithXSeparated, concatGroupedFieldValues(hblModel.containerVolumeGrouped, decimalPlaces));
        dictionary.put(ReportConstants.ContainerWeightGrouped, concatGroupedFields(hblModel.containerWeightGrouped, decimalPlaces));
        dictionary.put(ReportConstants.ContainerVolumeGrouped, concatGroupedFields(hblModel.containerVolumeGrouped, decimalPlaces));
        dictionary.put(ReportConstants.DELIVERY_AGENT, null);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(hblModel.commonContainers));
        if(hblModel.shipment != null && hblModel.shipment.getFreightLocal() != null)
            dictionary.put(ReportConstants.FREIGHT_LOCAL, hblModel.shipment.getFreightLocal());
        if(hblModel.shipment != null && hblModel.shipment.getFreightLocalCurrency() != null && !hblModel.shipment.getFreightLocalCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_LOCAL_CURRENCY, hblModel.shipment.getFreightLocalCurrency());
        if(hblModel.shipment != null && hblModel.shipment.getFreightOverseas() != null)
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, hblModel.shipment.getFreightOverseas());
        if(hblModel.shipment != null && hblModel.shipment.getFreightOverseasCurrency() != null && !hblModel.shipment.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, hblModel.shipment.getFreightOverseasCurrency());
        if(hblModel.shipment.getShipmentAddresses() != null && hblModel.shipment.getShipmentAddresses().size() > 0) {
            for (PartiesModel shipmentAddress: hblModel.shipment.getShipmentAddresses()) {
                if(shipmentAddress.getType() == CUSTOM_HOUSE_AGENT && shipmentAddress.getOrgData() != null && getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME) != null) {
                    dictionary.put(CHAPartyDescription, getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME));
                }
            }
        }

        AdditionalDetailModel additionalDetail = hblModel.shipment.getAdditionalDetails();
        CarrierDetailModel carrierDetails = hblModel.shipment.getCarrierDetails();

        if(hblModel.tenantSettingsResponse != null && hblModel.tenantSettingsResponse.isEnableIGMDetails())
        {
            if(hblModel.shipment.getDirection() != null && hblModel.shipment.getDirection() == Constants.IMP) {
                if(hblModel.shipment.getAdditionalDetails().getIGMFileDate() != null) {
                    dictionary.put(ReportConstants.IGM_FILE_DATE, hblModel.shipment.getAdditionalDetails().getIGMFileDate());
                }
                if(hblModel.shipment.getAdditionalDetails().getIGMFileNo() != null) {
                    dictionary.put(ReportConstants.IGM_FILE_NO, hblModel.shipment.getAdditionalDetails().getIGMFileNo());
                }
                if(hblModel.shipment.getAdditionalDetails().getIGMInwardDate() != null) {
                    dictionary.put(ReportConstants.IGM_INWARD_DATE, hblModel.shipment.getAdditionalDetails().getIGMInwardDate());
                }
                if(hblModel.shipment.getAdditionalDetails().getInwardDateAndTime() != null) {
                    dictionary.put(ReportConstants.INWARD_DATE_TIME, hblModel.shipment.getAdditionalDetails().getInwardDateAndTime());
                }
                if(hblModel.shipment.getAdditionalDetails().getLineNumber() != null) {
                    dictionary.put(ReportConstants.LINE_NUMBER, hblModel.shipment.getAdditionalDetails().getLineNumber());
                }
                if(hblModel.shipment.getAdditionalDetails().getSubLineNumber() != null) {
                    dictionary.put(ReportConstants.SUB_LINE_NUMBER, hblModel.shipment.getAdditionalDetails().getSubLineNumber());
                }
                if(hblModel.shipment.getAdditionalDetails().getIsInland()) {
                    dictionary.put(ReportConstants.IS_INLAND, hblModel.shipment.getAdditionalDetails().getIsInland()?"Yes":"No");
                    if(hblModel.shipment.getAdditionalDetails().getSMTPIGMDate() != null) {
                        dictionary.put(ReportConstants.SMTPIGM_DATE, hblModel.shipment.getAdditionalDetails().getSMTPIGMDate());
                    }
                    if(hblModel.shipment.getAdditionalDetails().getSMTPIGMNumber() != null) {
                        dictionary.put(ReportConstants.SMTPIGM_NUMBER, hblModel.shipment.getAdditionalDetails().getSMTPIGMNumber());
                    }
                    if(hblModel.shipment.getAdditionalDetails().getLocalLineNumber() != null) {
                        dictionary.put(ReportConstants.LOCAL_LINE_NUMBER, hblModel.shipment.getAdditionalDetails().getLocalLineNumber());
                    }
                }
            }
        }

        if(hblModel.consolidation != null ) {
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

        } else if (!Objects.isNull(hblModel.shipment) && !Objects.isNull(hblModel.shipment.getAdditionalDetails())) {
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
//            dictionary.put(AGENT_REFERENCE, hblModel.shipment)
            }
        }

        Optional<ReferenceNumbersModel> referenceNumber = Optional.empty();

        if(hblModel.shipment.getReferenceNumbersList() != null) {
            referenceNumber = hblModel.shipment.getReferenceNumbersList().stream().findFirst()
                    .filter(i -> i.getType().equals(ERN));
        }
        if(referenceNumber.isEmpty() && hblModel.consolidation != null) {
            referenceNumber = hblModel.consolidation.getReferenceNumbersList().stream().findFirst()
                    .filter(i -> i.getType().equals(ERN));
        }
        referenceNumber.ifPresent(i -> dictionary.put(EXPORT_REFERENCE_NUMBER, i.getReferenceNumber()));

        if(hblModel.shipment.getReferenceNumbersList() != null) {
            referenceNumber = hblModel.shipment.getReferenceNumbersList().stream().findFirst()
                    .filter(i -> i.getType().equals(SHIPMENT_CAN_DOCUMENT));
        }
        referenceNumber.ifPresent(i -> dictionary.put(CAN_NUMBER, i.getReferenceNumber()));

//        dictionary["BillRemarks"] = billRow != null ? billRow.Remarks : "";
//        List<BillChargesRow> originalChargesRows = new List<BillChargesRow>();
//        List<BillChargesRow> copyChargesRows = new List<BillChargesRow>();
        dictionary.put(AS_AGREED, false);
        dictionary.put(COPY_AS_AGREED, false);

        String chargesApply  = hblModel.shipment.getAdditionalDetails().getBLChargesDisplay();

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
//            getChargeRows(ref originalChargesRows, ref copyChargesRows, chargesRows, chargesApply);
        }
//        dictionary.put(CHARGES, originalChargesRows);
//        if(originalChargesRows != null && originalChargesRows.Count > 0)
//        {
//            String json = jsonHelper.convertToJson(originalChargesRows);
//            var values = jsonHelper.convertValue(json, new TypeReference<List<Map<String,Object>>>() {});
//            values.ForEach(v => {
//                if(v.get("OverseasSellAmount") != null){
//                    v.put("OverseasSellAmount", ReportHelper.addCommas(v.get("OverseasSellAmount").toString());
//                }});
//            dictionary.put("charges", values);
//        }
//        dictionary.put(COPY_CHARGES, copyChargesRows);

        PartiesModel notifyParty = hblModel.shipment.getAdditionalDetails().getNotifyParty();
        List<String> notifyPartyAddress = getOrgAddressWithPhoneEmail(notifyParty);
        if(notifyParty.getAddressData().get(FULL_NAME) != null) {
            notifyPartyAddress.add(0, getValueFromMap(notifyParty.getAddressData(), FULL_NAME));
        }
        dictionary.put(NOTIFY_PARTY_ADDRESS, notifyPartyAddress);

        if(!Objects.isNull(hblModel.shipment.getIsNotifyConsigneeEqual()) && hblModel.shipment.getIsNotifyConsigneeEqual()) {
            dictionary.put(NOTIFY_PARTY, "Same as Consignee");
            dictionary.put(NOTIFY_PARTY_CAPS, "SAME AS CONSIGNEE");
        } else {
            dictionary.put(NOTIFY_PARTY, dictionary.get(NOTIFY_PARTY_ADDRESS));
            dictionary.put(NOTIFY_PARTY_CAPS, notifyPartyAddress.stream().map(String::toUpperCase).toList());
        }

        List<String> consigner = null;
        List<String> consignee = null;
        if (hblModel.blObject != null & !hblModel.shipment.getTransportMode().equals(AIR)) {
            List<String> notify = getNotifyOrgAddress(hblModel.blObject);
            if (!Objects.isNull(notify)) {
                dictionary.put(BL_NOTIFY_PARTY, notify);
                dictionary.put(BL_NOTIFY_PARTY_CAPS, notify.stream().map(String::toUpperCase).toList());
            }
            consigner = getOrgAddress(hblModel.blObject.getHblData().getConsignorName(), hblModel.blObject.getHblData().getConsignorAddress(),
                    null, null, null, null);
            consignee = getOrgAddress(hblModel.blObject.getHblData().getConsigneeName(), hblModel.blObject.getHblData().getConsigneeAddress(),
                    null, null, null, null);
        } else {
            PartiesModel shipmentConsigner = hblModel.shipment.getConsigner();
            PartiesModel shipmentConsignee = hblModel.shipment.getConsignee();
            if(shipmentConsigner != null)
            {
                Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
                if(consignerAddress != null)
                {
                    consigner = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consignerAddress, COMPANY_NAME), getValueFromMap(consignerAddress, ADDRESS1),
                            getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                            getValueFromMap(consignerAddress,EMAIL), getValueFromMap(consignerAddress, CONTACT_PHONE),
                            getValueFromMap(consignerAddress,"Zip_PostCode"));
                    dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get(COMPANY_NAME));
                    dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get(CONTACT_PERSON));
                }
            }
            if(shipmentConsignee != null)
            {
                Map<String, Object> consigneeAddress = shipmentConsignee.getAddressData();
                if(consigneeAddress != null)
                {
                    consignee = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consigneeAddress, COMPANY_NAME), getValueFromMap(consigneeAddress, ADDRESS1),
                            getValueFromMap(consigneeAddress, ADDRESS2),
                            ReportHelper.getCityCountry(getValueFromMap(consigneeAddress, CITY), getValueFromMap(consigneeAddress, COUNTRY)),
                            getValueFromMap(consigneeAddress,EMAIL), getValueFromMap(consigneeAddress, CONTACT_PHONE),
                            getValueFromMap(consigneeAddress,"Zip_PostCode"));
                    dictionary.put(ReportConstants.CONSIGNEE_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                    dictionary.put(ReportConstants.CONSIGNEE_CONTACT_PERSON,getValueFromMap(consigneeAddress,CONTACT_PERSON));
                }
            }
        }



        List<String> deliveryParty = getOrgAddress(hblModel.shipment.getAdditionalDetails().getNotifyParty());
        dictionary.put(CONSIGNER, consigner);
        dictionary.put(CONSIGNER_CAPS, consigner != null ? consigner.stream().map(String::toUpperCase).toList() : null);
        dictionary.put(CONSIGNER_ADDRESS, getAddressList(hblModel.blObject.getHblData().getConsignorAddress()));
        dictionary.put(NOTIFY_PARTY_ADDRESS, getAddressList(hblModel.blObject.getHblNotifyParty().get(0).getAddress()));

        String description = hblModel.blObject.getHblData().getCargoDescription();
        description = description != null ? description : hblModel.shipment.getGoodsDescription();
        dictionary.put(DESCRIPTION, description);
        if (!Objects.isNull(description))
            dictionary.put(DESCRIPTION_CAPS, description.toUpperCase());
        dictionary.put(DESCRIPTION_ORIGINAL, getAddressList(description));
        dictionary.put(CONSIGNEE, consignee);
        dictionary.put(CONSIGNEE_CAPS, consignee.stream().map(String::toUpperCase).collect(Collectors.toList()));

        dictionary.put(NOTIFY_PARTY_FREETEXT, dictionary.get(NOTIFY_PARTY));
        dictionary.put(CONSIGNEE_FREETEXT, dictionary.get(CONSIGNEE));
        dictionary.put(CONSIGNER_FREETEXT, dictionary.get(CONSIGNER));

        dictionary.put(ORIGINAL_OR_COPY, ORIGINAL);
        if (! Objects.isNull(hblModel.tenant)) {
            dictionary.put(TENANT_NAME, hblModel.tenant.tenantName);
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
        dictionary.put(BL_VESSEL_NAME, hblModel.blObject.getHblData().getVesselName());
        dictionary.put(BL_VOYAGE, hblModel.blObject.getHblData().getVoyage());

        // SHIPMENT FIELDS
        dictionary.put(ENTRY_REF_NUMBER, hblModel.shipment.getEntryRefNo());
        dictionary.put(VESSEL_NAME, hblModel.shipment.getCarrierDetails().getVessel());
        dictionary.put(VOYAGE, hblModel.shipment.getCarrierDetails().getVoyage());
        dictionary.put(TRANSPORT_MODE, hblModel.shipment.getTransportMode());
        dictionary.put(DESCRIPTION, hblModel.blObject != null ? hblModel.blObject.getHblData().getCargoDescription()
                : hblModel.shipment.getGoodsDescription());
        dictionary.put(CHARGEABLE, hblModel.shipment.getChargable());
        dictionary.put(CHARGEABLE_UNIT, hblModel.shipment.getChargeableUnit());
        dictionary.put(FREIGHT_OVERSEAS, hblModel.shipment.getFreightOverseas());
        dictionary.put(FREIGHT_OVERSEAS_CURRENCY, hblModel.shipment.getFreightOverseasCurrency());
        dictionary.put(ORIGINALS, hblModel.shipment.getAdditionalDetails().getOriginal());
        dictionary.put(ORIGINAL_WORDS, numberToWords(hblModel.shipment.getAdditionalDetails().getOriginal()));
        dictionary.put(ISSUE_PLACE_NAME, hblModel.placeOfIssue != null ? hblModel.placeOfIssue.getName() : "");
        dictionary.put(ISSUE_PLACE_COUNTRY, hblModel.placeOfIssue != null ? hblModel.placeOfIssue.getCountry() : "");
        dictionary.put(ISSUEPLACECOUNTRYNAME, hblModel.issuePlaceCountry); //MasterData
        dictionary.put(BL_COMMENTS, hblModel.blObject.getHblData().getBlComments());
        dictionary.put(MARKS_AND_NUMBER, hblModel.blObject.getHblData().getMarksAndNumbers());
        if (!Objects.isNull(hblModel.blObject.getHblData().getMarksAndNumbers()))
            dictionary.put(MARKS_N_NUMS_CAPS, hblModel.blObject.getHblData().getMarksAndNumbers().toUpperCase());
//        dictionary.put(SHIPPED_ON_BOARD, hblModel.shipeedOnBoard != null ? ConvertToDPWDateFormat(hblModel.shipeedOnBoard) : null);
        dictionary.put(PACKS, hblModel.blObject.getHblData().getPackageCount());
        dictionary.put(PACKS_UNIT, hblModel.blObject.getHblData().getPackageType());
        dictionary.put(PACKS_UNIT_DESC, masterListDescriptionPacksUnit(hblModel.blObject.getHblData().getPackageType()));
        dictionary.put(SHIPMENT_CONTAINERS, hblModel.shipment.getShipmentContainersList());
        dictionary.put(IS_IMPORT, hblModel.shipment.getDirection().equals(IMP));

        dictionary.put(DELIVERY_PARTY, deliveryParty);
        dictionary.put(DELIVERY_PHONE, getValueFromMap(notifyParty.getAddressData(), MOBILE));
        dictionary.put(DELIVERY_FAX, getValueFromMap(notifyParty.getAddressData(), FAX));
        dictionary.put(LOGO, getLogoPath(hblModel.user));

        if (!Objects.isNull(hblModel.shipment.getShipmentContainersList())) {
            int containerCount = 0;
            for (var container : hblModel.shipment.getShipmentContainersList()) {
                containerCount += container.getContainerCount();
            }
            dictionary.put(CONTAINER_COUNT, numberToWords(containerCount).toUpperCase());
        }

        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        dictionary.put(HOUSE_BILL, hblModel.shipment.getHouseBill());
//        dictionary.put(SUMMARY, hblModel.shipment.getSummary);
        dictionary.put(SHIPMENT_ID, hblModel.shipment.getShipmentId());
        dictionary.put(REFERENCE_NO, hblModel.shipment.getBookingReference());
        dictionary.put(SERVICE_MODE_DESCRIPTION, hblModel.serviceMode); //MasterListData
//        dictionary.put("Goods_CO", hblModel.); //MasterListData
        dictionary.put(PAYMENT_TERMS, hblModel.paymentTerms); //MasterListData
        dictionary.put(RELEASE_TYPE, hblModel.releaseType); //MasterListData


        if (hblModel.shipment.getCarrierDetails().getEtd() != null)
            dictionary.put(ETD,ConvertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getEtd()));
        if (hblModel.shipment.getCarrierDetails().getEta() != null)
            dictionary.put(ETA,ConvertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getEta()));
        if (hblModel.shipment.getAdditionalDetails().getDateOfIssue() != null){
            dictionary.put(DATE_OF_ISSUE_MDY, ConvertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getDateOfIssue()));
            dictionary.put(DATE_OF_ISSUE_DMY, DateTimeFormatter.ofPattern("dd/MM/yyyy").format(hblModel.shipment.getAdditionalDetails().getDateOfIssue()));
            dictionary.put(DATE_OF_ISSUE_DMMY, DateTimeFormatter.ofPattern("dd-MMM-yyyy").format(hblModel.shipment.getAdditionalDetails().getDateOfIssue()));
        }
        if (hblModel.shipment.getAdditionalDetails().getDateOfReceipt() != null)
            dictionary.put(DATE_OF_RECEIPT ,ConvertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getDateOfReceipt()));
        if (hblModel.shipment.getCarrierDetails().getAtd() != null) {
            LocalDateTime atd = hblModel.shipment.getCarrierDetails().getAtd();
            dictionary.put(ATD_MDY, ConvertToDPWDateFormat(atd));
            dictionary.put(ATD_DMY, DateTimeFormatter.ofPattern("dd/MM/yyyy").format(atd));
            dictionary.put(ATD_DMMY, DateTimeFormatter.ofPattern("dd-MMM-yyyy").format(atd));
        }

        if (hblModel.shipment.getDeliveryDetails().getActualPickupOrDelivery() != null)
            dictionary.put(ACTUAL_DELIVERY, DateTimeFormatter.ofPattern(GetDPWDateFormatOrDefault() + " hh:mm tt")
                    .format(hblModel.shipment.getDeliveryDetails().getActualPickupOrDelivery()));

        dictionary.put(SHIPPER_REF_NO, hblModel.shipment.getPickupDetails().getShipperRef());
        MathContext precision = new MathContext(decimalPlaces);
        BigDecimal weight = hblModel.shipment.getWeight().round(precision);
        BigDecimal volume = hblModel.shipment.getVolume().round(precision);
        BigDecimal chargeable = hblModel.shipment.getChargable().round(precision);

        dictionary.put(WEIGHT, weight);
        dictionary.put(VOLUME, volume);
        dictionary.put(CHARGEABLE, chargeable);
        dictionary.put(WEIGHT_AND_UNIT, String.format("%s %s", weight, hblModel.shipment.getWeightUnit()));
        dictionary.put(VOLUME_AND_UNIT, String.format("%s %s", volume, hblModel.shipment.getVolumeUnit()));
        dictionary.put(CHARGEABLE_AND_UNIT, String.format("%s %s", chargeable, hblModel.shipment.getChargeableUnit()));
//        dictionary.put(DELIVERY_TO_EMAIL_ADDRESS, DeliveryEmailAddress);
        dictionary.put(PLACE_OF_DELIVERY, hblModel.podCountry);
        dictionary.put(BL_PLACE_OF_DELIVERY, hblModel.blObject.getHblData().getPlaceOfDelivery());
        dictionary.put(BL_WEIGHT, hblModel.blObject.getHblData().getCargoGrossWeight());
        dictionary.put(BL_WEIGHT_UNIT, hblModel.blObject.getHblData().getCargoGrossWeightUnit());
        dictionary.put(BL_NETWEIGHT, hblModel.blObject.getHblData().getCargoNetWeight());
        dictionary.put(BL_NETWEIGHT_UNIT, hblModel.blObject.getHblData().getCargoNetWeightUnit());
        dictionary.put(BL_DELIVERYAGENT, hblModel.blObject.getHblData().getDeliveryAgent());
        dictionary.put(BL_DELIVERYAGENT_ADDRESS, hblModel.blObject.getHblData().getDeliveryAgentAddress());
        dictionary.put(BL_CARGO_TERMS_DESCRIPTION, hblModel.blObject.getHblData().getBlRemarksDescription());
        dictionary.put(BL_REMARKS_DESCRIPTION, hblModel.blObject.getHblData().getBlRemarksDescription());

        PartiesModel deliveryToAddress =  hblModel.shipment.getDeliveryDetails().getDestinationDetail();
        if (deliveryToAddress != null && deliveryToAddress.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryToAddress.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryTo");
            var deliveryTo = getOrgAddress(getValueFromMap(addressMap, COMPANY_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryTo", deliveryTo);
        }

        PartiesModel deliveryAgentAddress =  hblModel.shipment.getDeliveryDetails().getAgentDetail();
        if (deliveryAgentAddress != null && deliveryAgentAddress.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryAgentAddress.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryAgent");
            var deliveryAgent = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryAgent", deliveryAgent);
        }

        PartiesModel deliveryTransportCompany =  hblModel.shipment.getDeliveryDetails().getTransporterDetail();
        if (deliveryTransportCompany != null && deliveryTransportCompany.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryTransportCompany.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryTransport");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryTransport", address);
        }

        PartiesModel deliveryCfs =  hblModel.shipment.getDeliveryDetails().getSourceDetail();
        if (deliveryCfs != null && deliveryCfs.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryCfs.getAddressData();
            populateAddress(addressMap, dictionary, "DeliveryCfs");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("DeliveryCfs", address);
        }

        PartiesModel pickupFrom =  hblModel.shipment.getPickupDetails().getSourceDetail();
        if (pickupFrom != null && pickupFrom.getAddressData() != null)
        {
            Map<String, Object> addressMap = pickupFrom.getAddressData();
            populateAddress(addressMap, dictionary, "PickupFrom");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("PickupFrom", address);
        }

        PartiesModel pickupTransportCompany = hblModel.shipment.getPickupDetails().getTransporterDetail();
        if (pickupTransportCompany != null && pickupTransportCompany.getAddressData() != null)
        {
            Map<String, Object> addressMap = pickupTransportCompany.getAddressData();
            populateAddress(addressMap, dictionary, "PickupTransport");
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put("PickupTransport", address);
        }

        PartiesModel pickupCfs =  hblModel.shipment.getPickupDetails().getDestinationDetail();
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

        dictionary.put(USER_FULLNAME, hblModel.user.getDisplayName());
        dictionary.put(USER_NAME, hblModel.user.getUsername());
        dictionary.put(USER_EMAIL, hblModel.user.getEmail());

        String onBoard = hblModel.shipment.getAdditionalDetails().getOnBoard();
        if( onBoard != null && !onBoard.isEmpty()){
            String OnBoardValue = null;
            if(onBoard.equalsIgnoreCase("SHP")) {
                OnBoardValue = "Shipped On Board";
            }
            if(onBoard.equalsIgnoreCase("RFS")) {
                OnBoardValue = "Received For Shipment";
            }
            dictionary.put(ONBOARD_DATE, OnBoardValue);
            dictionary.put(ONBOARD_TYPE_DATE, hblModel.shipment.getAdditionalDetails().getOnBoardDate() != null ?
                    ConvertToDPWDateFormat(hblModel.shipment.getAdditionalDetails().getOnBoardDate()) : null);
        }
// TODO
//        if(!String.IsNullOrEmpty(PrintType)) {
//            if(PrintType.ToUpper() == "ORIGINAL") {
//                dictionary.put(IS_ORIGINAL, true);
//            } else {
//                dictionary.put(IS_ORIGINAL, false);
//            }
//        }

        if (hblModel.shipment.getCarrierDetails().getAtd() != null)
            dictionary.put(ATD, ConvertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getAtd()));
        if (hblModel.shipment.getCarrierDetails().getAta() != null)
            dictionary.put(ATA, ConvertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getAta()));

        dictionary.put(ATTENTION, dictionary.get(CONSIGNEE));
        dictionary.put(DO_NO, hblModel.shipment.getShipmentId());
        if (!Objects.isNull(hblModel.shipment.getConsignee()) && !Objects.isNull(hblModel.shipment.getConsignee().getAddressData()))
            dictionary.put(MESSERS, getValueFromMap(hblModel.shipment.getConsignee().getAddressData(), FULL_NAME));
        dictionary.put(IGM_NO, hblModel.shipment.getAdditionalDetails().getIGMFileNo());
        dictionary.put(FLIGHT_NAME, hblModel.shipment.getCarrierDetails().getShippingLine());
        dictionary.put(FLIGHT_NUMBER, hblModel.shipment.getCarrierDetails().getFlightNumber());
        dictionary.put(MBL_NUMBER, hblModel.shipment.getMasterBill());
        dictionary.put(HBL_NUMBER, hblModel.shipment.getHouseBill());

        if(hblModel.shipment.getVolumetricWeight() != null)
            dictionary.put(V_WEIGHT_AND_UNIT_AIR, String.format("%s %s", ReportHelper.twoDecimalPlacesFormat(
                    hblModel.shipment.getVolumetricWeight().toString()), hblModel.shipment.getVolumetricWeightUnit()));
        if(hblModel.shipment.getWeight() != null)
            dictionary.put(WEIGHT_AND_UNIT_AIR, String.format("%s %s", ReportHelper.twoDecimalPlacesFormat(
                    hblModel.shipment.getWeight().toString()), hblModel.shipment.getWeightUnit()));
        if(hblModel.shipment.getVolume() != null)
            dictionary.put(VOLUME_AND_UNIT_AIR, String.format("%s %s", ReportHelper.twoDecimalPlacesFormat(
                    hblModel.shipment.getVolume().toString()), hblModel.shipment.getVolumeUnit()));

        dictionary.put(DESC_OF_GOODS, hblModel.shipment.getGoodsDescription());
        dictionary.put(JOB_NO, hblModel.shipment.getShipmentId());
        dictionary.put(FLIGHT_CARRIER, hblModel.shipment.getCarrierDetails().getShippingLine());
        dictionary.put(PORT_OF_LOADING, hblModel.polPort != null ? hblModel.polPort.getPortName() : null);
        dictionary.put(PORT_OF_LOADING_COUNTRY, hblModel.polPort != null ? hblModel.polPort.getCountry() : null);
        dictionary.put(PORT_OF_DISCHARGE, hblModel.podPort != null ? hblModel.podPort.getPortName() : null);
        dictionary.put(PORT_OF_DISCHARGE_COUNTRY, hblModel.podPort != null ? hblModel.podPort.getCountry() : null);
        dictionary.put(PLACE_oF_RECEIPT, hblModel.shipment.getCarrierDetails().getOrigin());
        dictionary.put(PLACE_oF_DELIVERY, hblModel.shipment.getCarrierDetails().getDestination());
        dictionary.put(PORT_OF_FINAL_DESTINATION, hblModel.podPort != null ? hblModel.podPort.getPortName() : null);
        dictionary.put(PORT_OF_FINAL_DESTINATION_COUNTRY, hblModel.podPort != null ? hblModel.podPort.getCountry() : null);
        dictionary.put(TRANSPORT_MODE, hblModel.shipment.getTransportMode());

        ConsolidationModel consolidation = getFirstConsolidationFromShipmentId(hblModel.shipment.getId());
        if(consolidation != null && consolidation.getPayment() != null)
            dictionary.put(PPCC, consolidation.getPayment());

        if(hblModel.shipment.getPickupDetails().getActualPickupOrDelivery() != null)
            dictionary.put(STATUS, CONFIRMED);
        else
            dictionary.put(STATUS, PLANNED);

        if(!Objects.isNull(hblModel.shipment.getPackingList()) && !hblModel.shipment.getPackingList().isEmpty())
        {
            var values = hblModel.shipment.getPackingList().stream()
                    .map(i -> jsonHelper.convertJsonToMap(jsonHelper.convertToJson(i)))
                    .toList();
            values.forEach(v -> {
                JsonDateFormat(v);
                if(v.containsKey(COMMODITY_NAME))
                    v.put(COMMODITY_DESC, v.get(COMMODITY_NAME).toString());
                if(v.get(WEIGHT) != null){
                    v.put(WEIGHT_AND_UNIT_PACKS, String.format("%s %s", twoDecimalPlacesFormat(v.get(WEIGHT).toString()),
                            v.get(WEIGHT_UNIT)));
                }
                if(v.get(VOLUME) != null){
                    v.put(VOLUME_AND_UNIT_PACKS, String.format("%s %s", twoDecimalPlacesFormat(v.get(VOLUME).toString()),
                            v.get(VOLUME_UNIT)));
                }
                if(v.get(VOLUME_WEIGHT) != null){
                    v.put(V_WEIGHT_AND_UNIT_PACKS, String.format("%s %s", twoDecimalPlacesFormat(v.get(VOLUME_WEIGHT).toString()),
                            v.get(WEIGHT_UNIT)));
                }
                if(hblModel.shipment.getPickupDetails().getActualPickupOrDelivery() != null) {
                    v.put(LOADED_DATE, ConvertToDPWDateFormat(hblModel.shipment.getPickupDetails().getActualPickupOrDelivery()));
                }
            });
            dictionary.put(PACKS_DETAILS, values);
        }

        dictionary.put(PICKUP_ORDER_CONTACT_PERSON, EMPTY_STRING);
        if (pickupTransportCompany != null && pickupTransportCompany.getAddressData() != null) {
            if (getValueFromMap(pickupTransportCompany.getAddressData(), CONTACT_PERSON) != null) {
                dictionary.put(PICKUP_ORDER_CONTACT_PERSON, getValueFromMap(pickupTransportCompany.getAddressData(), CONTACT_PERSON));
            }
        }

        dictionary.put(CONTAINER_SUMMARY, EMPTY_STRING);
        if(hblModel.shipment.getShipmentContainersList() != null) {
            Map<String, Long> containerSummary =  new HashMap<>();
            for(var container : hblModel.shipment.getShipmentContainersList()) {
                String code = container.getContainerTypeCode();
                Long count = container.getContainerCount();
                Long previousCount = 0L;
                if(containerSummary.get(code)  != null)
                    previousCount = containerSummary.get(code);

                containerSummary.put(code,  previousCount + count);
            }
            var containerCounts = String.join(",", containerSummary.entrySet().stream()
                    .map(i -> String.format("%s * %s", i.getValue(), i.getKey())).toList());
            dictionary.put(CONTAINER_SUMMARY, containerCounts);
        }

        dictionary.put(BOOKING_NUMBER, hblModel.shipment.getBookingNumber());
        dictionary.put(ADDITIONAL_TERMS, hblModel.shipment.getAdditionalTerms());
        dictionary.put(VESSEL_BERTHING_DATE, ConvertToDPWDateFormat(hblModel.shipment.getCarrierDetails().getVesselBerthingDate()));

        dictionary.put(UCR_REFERENCE, EMPTY_STRING);
        dictionary.put(EMPTY_TRUCK_IN_DATE, EMPTY_STRING);
        dictionary.put(LOADED_TRUCK_GATE_OUT_DATE, EMPTY_STRING);
        if (hblModel.shipment.getPickupDetails() != null)
        {
            PickupDeliveryDetailsModel pickupDetails = hblModel.shipment.getPickupDetails();
            dictionary.put(UCR_REFERENCE, pickupDetails.getUcrReference());
            dictionary.put(EMPTY_TRUCK_IN_DATE, ConvertToDPWDateFormat(pickupDetails.getEmptyTruckInDate()));
            dictionary.put(LOADED_TRUCK_GATE_OUT_DATE, ConvertToDPWDateFormat(pickupDetails.getLoadedTruckGateOutDate()));
            dictionary.put(PICKUP_PORT_TRANSPORT_ADVISED, ConvertToDPWDateFormat(pickupDetails.getPortTransportAdvised()));
        }

        List<String> bookingPreCarriageMode = new ArrayList<>();
        List<String> bookingCarriageVesselVoyage = new ArrayList<>();

        if(hblModel.shipment.getBookingCarriagesList() != null){
            for(var bookingCarriage : hblModel.shipment.getBookingCarriagesList()) {
                if(bookingCarriage.getCarriageType().equals(PRE_CARRIAGE)){
                    bookingCarriageVesselVoyage.add(bookingCarriage.getVessel());
                    bookingCarriageVesselVoyage.add(bookingCarriage.getVoyage());
                }
            }
        }

        //TODO populate bookingPreCarriageMode
        if (bookingPreCarriageMode.size() > 0)
            dictionary.put(PRE_CARRIAGE_MODE, String.join(",", bookingPreCarriageMode));
        if (bookingCarriageVesselVoyage.size() > 0)
            dictionary.put(PRE_CARRIAGE_VESSEL_VOYAGE, String.join(",", bookingCarriageVesselVoyage));

        // ====================  END OF MIGRATION PLACEHOLDER ===================
        populateShipmentFields(hblModel.shipment, false, dictionary);
        populateConsolidationFields(hblModel.consolidation, dictionary);
        populateBlFields(hblModel.blObject, dictionary);
        dictionary.put(ReportConstants.PAID_PLACE_COUNTRY_NAME, hblModel.paidPlaceCountry);
        dictionary.put(ReportConstants.SERVICE_MODE_DESCRIPTION, hblModel.serviceMode);
        dictionary.put(ReportConstants.PPCC, hblModel.paymentTerms);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(hblModel.commonContainers));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, hblModel.commonContainers);
        dictionary.put(ReportConstants.PRE_CARRIAGE, hblModel.preCarriageVessel != null ? hblModel.preCarriageVessel.getName() : null);
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
        if(delivery != null && delivery.getAgentDetail() != null && delivery.getAgentDetail().getAddressData() != null)
        {
            Map<String, Object> address = delivery.getAgentDetail().getAddressData();
            dictionary.put(ReportConstants.DELIVERY_AGENT, ReportHelper.getOrgAddressWithPhoneEmail(
                    StringUtility.convertToString(address.get(COMPANY_NAME)), StringUtility.convertToString(address.get(ReportConstants.ADDRESS1)), StringUtility.convertToString(address.get(ReportConstants.ADDRESS2)),
                    StringUtility.convertToString(address.get(ReportConstants.COUNTRY)), StringUtility.convertToString(address.get(ReportConstants.EMAIL)), StringUtility.convertToString(address.get(ReportConstants.CONTACT_PHONE)),
                    null
            ));
        }
        return dictionary;
    }

    // isActive Criteria not clear from v1 impl
    private String masterListDescriptionPacksUnit(String packageType) {
        if(packageType == null || packageType.isEmpty())
            return packageType;

        MasterData masterData = getMasterListData(MasterDataType.PAYMENT, packageType);
        return  (masterData != null ? masterData.getItemDescription() : null);
    }

    private String getLogoPath(UsersDto user) {
        String basePath = "Upload/";
        var path = basePath + user.TenantId + "/Assets/" + user.HouseBillLogo;
        if (user.HouseBillLogo != null && !user.HouseBillLogo.isEmpty()) {
            return path;
        }
        return null;
    }

    private void populateAddress(Map<String, Object> addressData, Map<String, Object> dictionary, String prefix) {
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

    private List<String> getNotifyOrgAddress(Hbl hbl)
    {
        if(hbl != null && hbl.getHblNotifyParty() != null) {
            HblPartyDto row = hbl.getHblNotifyParty().get(0);
            getOrgAddress(row.getName(), row.getAddress(), null, null, row.getEmail(), null);
        }
        return null;
    }
}

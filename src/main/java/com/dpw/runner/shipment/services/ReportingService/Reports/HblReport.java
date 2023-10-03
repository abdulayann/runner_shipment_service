package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

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
            paidPlace = unlocationsResponse.get(0);
        masterData = getMasterListData(MasterDataType.COUNTRIES, paidPlace.getCountry());
        hblModel.paidPlaceCountry = (masterData != null ? masterData.getItemDescription() : null);
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
                hblModel.noofPackages = container.getNoOfPackages() + hblModel.noofPackages;
                hblModel.containerCountGrouped = new HashMap<>();
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
        if(pickup != null && pickup.getTransporterDetail() != null)
        {
            Map<String, Object> address = pickup.getTransporterDetail().getAddressData();
            dictionary.put(ReportConstants.PICKUP_TRANSPORT, ReportHelper.getOrgAddressWithPhoneEmail(
                    address.get("OrgFullName").toString(), address.get(ReportConstants.ADDRESS1).toString(), address.get(ReportConstants.ADDRESS2).toString(),
                    address.get(ReportConstants.COUNTRY).toString(), address.get(ReportConstants.EMAIL).toString(), address.get(ReportConstants.CONTACT_PHONE).toString(),
                            null
            ));
        }
        PickupDeliveryDetailsModel delivery = hblModel.shipment.getDeliveryDetails();
        if(delivery != null && delivery.getAgentDetail() != null)
        {
            Map<String, Object> address = delivery.getAgentDetail().getAddressData();
            dictionary.put(ReportConstants.DELIVERY_AGENT, ReportHelper.getOrgAddressWithPhoneEmail(
                    address.get("OrgFullName").toString(), address.get(ReportConstants.ADDRESS1).toString(), address.get(ReportConstants.ADDRESS2).toString(),
                    address.get(ReportConstants.COUNTRY).toString(), address.get(ReportConstants.EMAIL).toString(), address.get(ReportConstants.CONTACT_PHONE).toString(),
                    null
            ));
        }
        return dictionary;
    }
}

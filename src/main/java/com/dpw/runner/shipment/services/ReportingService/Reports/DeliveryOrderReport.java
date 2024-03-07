package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.DeliveryOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class DeliveryOrderReport extends IReport{

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        DeliveryOrderModel deliveryOrderModel = (DeliveryOrderModel) getDocumentModel(id);
        return populateDictionary(deliveryOrderModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        DeliveryOrderModel deliveryOrderModel = new DeliveryOrderModel();
        deliveryOrderModel.shipmentDetails = getShipment(id);
        deliveryOrderModel.usersDto = UserContext.getUser();
        if(deliveryOrderModel.shipmentDetails.getConsolidationList() != null && deliveryOrderModel.shipmentDetails.getConsolidationList().size() > 0)
        {
            deliveryOrderModel.consolidationDetails = deliveryOrderModel.shipmentDetails.getConsolidationList().get(0);
            UnlocationsResponse placeOfIssue = null;
            if(StringUtility.isNotEmpty(deliveryOrderModel.consolidationDetails.getPlaceOfIssue()))
            {
                List<Object> criteria = Arrays.asList(
                        Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                        "=",
                        deliveryOrderModel.consolidationDetails.getPlaceOfIssue()
                );
                CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
                V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
                List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
                if(unlocationsResponse.size() > 0)
                    placeOfIssue = unlocationsResponse.get(0);
                if(placeOfIssue != null)
                {
                    deliveryOrderModel.placeOfIssueName = placeOfIssue.getNameWoDiacritics();
                }
            }
        }
        deliveryOrderModel.setContainers(new ArrayList<>());
        if(deliveryOrderModel.shipmentDetails.getContainersList() != null)
        {
            for(ContainerModel container : deliveryOrderModel.shipmentDetails.getContainersList())
                deliveryOrderModel.getContainers().add(getShipmentContainer(container));
        }
        MasterData masterData = getMasterListData(MasterDataType.PAYMENT, deliveryOrderModel.shipmentDetails.getPaymentTerms());
        deliveryOrderModel.paymentTerms = (masterData != null ? masterData.getItemDescription() : null);
        deliveryOrderModel.hbl = getHbl(id);
        return deliveryOrderModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        DeliveryOrderModel deliveryOrderModel = (DeliveryOrderModel) documentModel;
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(deliveryOrderModel.shipmentDetails, GetDPWDateFormatOrDefault());
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(deliveryOrderModel.shipmentDetails, dictionary);
        populateConsolidationFields(deliveryOrderModel.consolidationDetails, dictionary);
        populateUserFields(deliveryOrderModel.usersDto, dictionary);
        populateBlFields(deliveryOrderModel.hbl, dictionary);
        populateBillChargesFields(deliveryOrderModel.shipmentDetails, dictionary);
        populateShipmentOrganizationsLL(deliveryOrderModel.shipmentDetails, dictionary);
        dictionary.put(ReportConstants.MASTER_BILL_ISSUE_PLACE, deliveryOrderModel.placeOfIssueName);
        dictionary.put(ReportConstants.PPCC, deliveryOrderModel.paymentTerms);

        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        dictionary.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(deliveryOrderModel.shipmentDetails.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.VOLUME, ConvertToVolumeNumberFormat(deliveryOrderModel.shipmentDetails.getVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CHARGEABLE, ConvertToWeightNumberFormat(deliveryOrderModel.shipmentDetails.getChargable(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.NetWeight, ConvertToWeightNumberFormat(deliveryOrderModel.shipmentDetails.getNetWeight(), v1TenantSettingsResponse));
        if(deliveryOrderModel.getContainers() != null && deliveryOrderModel.getContainers().size() > 0) {
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ShipmentContainers shipmentContainers : deliveryOrderModel.getContainers()) {
                String shipContJson = jsonHelper.convertToJson(shipmentContainers);
                valuesContainer.add(jsonHelper.convertJsonToMap(shipContJson));
            }
            for (Map<String, Object> v : valuesContainer) {
                if(v.containsKey(ReportConstants.GROSS_VOLUME) && v.get(ReportConstants.GROSS_VOLUME) != null)
                    v.put(ReportConstants.GROSS_VOLUME, ConvertToVolumeNumberFormat(v.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.GROSS_WEIGHT) && v.get(ReportConstants.GROSS_WEIGHT) != null)
                    v.put(ReportConstants.GROSS_WEIGHT, ConvertToWeightNumberFormat(v.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.NetWeight) && v.get(ReportConstants.NetWeight) != null)
                    v.put(ReportConstants.NetWeight, ConvertToWeightNumberFormat(v.get(ReportConstants.NetWeight), v1TenantSettingsResponse));
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
        }

        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(deliveryOrderModel.getContainers()));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, deliveryOrderModel.getContainers());

        //Add P0 tags
        PickupDeliveryDetailsModel deliveryDetails = deliveryOrderModel.shipmentDetails.getDeliveryDetails();
        if(deliveryDetails != null) {
            LocalDateTime deliveryTime = deliveryDetails.getActualPickupOrDelivery() != null ? deliveryDetails.getActualPickupOrDelivery() :
                    deliveryDetails.getEstimatedPickupOrDelivery();
            dictionary.put(DELIVERY_TIME,  ConvertToDPWDateFormatWithTime(deliveryTime, v1TenantSettingsResponse.getDPWDateFormat(), true));
        }

        if(!Objects.isNull(deliveryOrderModel.shipmentDetails.getPackingList()) && !deliveryOrderModel.shipmentDetails.getPackingList().isEmpty()) {
            getPackingDetails(deliveryOrderModel.shipmentDetails, dictionary);
            dictionary.put(HAS_PACK_DETAILS, true);
            var hazardousCheck = deliveryOrderModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getHazardous()) && x.getHazardous());
            var temperatureCheck = deliveryOrderModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getIsTemperatureControlled()) && x.getIsTemperatureControlled());
            if (hazardousCheck)
                dictionary.put(HAS_DANGEROUS_GOODS, true);
            else
                dictionary.put(HAS_DANGEROUS_GOODS, false);
            if (temperatureCheck)
                dictionary.put(HAS_TEMPERATURE_DETAILS, true);
            else
                dictionary.put(HAS_TEMPERATURE_DETAILS, false);

        } else {
            dictionary.put(HAS_PACK_DETAILS, false);
        }

//        getPackingDetails(deliveryOrderModel.shipmentDetails, dictionary);

        if(dictionary.containsKey(CHARGES_SMALL) && dictionary.get(CHARGES_SMALL) instanceof List){
            List<Map<String, Object>> values = (List<Map<String, Object>>)dictionary.get(CHARGES_SMALL);
            for (Map<String, Object> v: values) {
                if(v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, GetChargeTypeDescriptionLL((String)v.get(CHARGE_TYPE_CODE)));
                }
            }
        }

        return dictionary;
    }
}

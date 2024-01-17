package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.DeliveryOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Parties;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DELIVERY_TIME;

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
        v1ServiceUtil.validateCreditLimit(modelMapper.map(deliveryOrderModel.shipmentDetails.getClient(), Parties.class), Constants.DO_PRINT, deliveryOrderModel.shipmentDetails.getGuid());
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
        deliveryOrderModel.containers = new ArrayList<>();
        if(deliveryOrderModel.shipmentDetails.getContainersList() != null)
        {
            for(ContainerModel container : deliveryOrderModel.shipmentDetails.getContainersList())
                deliveryOrderModel.containers.add(getShipmentContainer(container));
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
        populateShipmentFields(deliveryOrderModel.shipmentDetails, false, dictionary);
        populateConsolidationFields(deliveryOrderModel.consolidationDetails, dictionary);
        populateUserFields(deliveryOrderModel.usersDto, dictionary);
        populateBlFields(deliveryOrderModel.hbl, dictionary);
        dictionary.put(ReportConstants.MASTER_BILL_ISSUE_PLACE, deliveryOrderModel.placeOfIssueName);
        dictionary.put(ReportConstants.PPCC, deliveryOrderModel.paymentTerms);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(deliveryOrderModel.containers));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, deliveryOrderModel.containers);

        //Add P0 tags
        PickupDeliveryDetailsModel deliveryDetails = deliveryOrderModel.shipmentDetails.getDeliveryDetails();
        if(deliveryDetails != null) {
            LocalDateTime deliveryTime = deliveryDetails.getActualPickupOrDelivery() != null ? deliveryDetails.getActualPickupOrDelivery() :
                    deliveryDetails.getEstimatedPickupOrDelivery();
            dictionary.put(DELIVERY_TIME,  ConvertToDPWDateFormat(deliveryTime));
        }

        getPackingDetails(deliveryOrderModel.shipmentDetails, dictionary);


        return dictionary;
    }
}

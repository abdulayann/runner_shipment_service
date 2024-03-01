package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.AWbLabelModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Component
public class AWBLabelReport extends IReport{
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;
    @Override
    public Map<String, Object> getData(Long id) {
        AWbLabelModel awbLabelModel = (AWbLabelModel) getDocumentModel(id);
        return populateDictionary(awbLabelModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        AWbLabelModel awbLabelModel = new AWbLabelModel();
        awbLabelModel.shipment = getShipment(id);

        // TODO TenantRow required
        return awbLabelModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        AWbLabelModel awbLabelModel = (AWbLabelModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        String mawb = awbLabelModel.shipment.getMasterBill();
        if(mawb != null){
            mawb = mawb.replace("-","");
            if(mawb.length() < 11) mawb = appendZero(mawb, 11);
            if(mawb.length() >= 3) dictionary.put(ReportConstants.MAWB13, mawb.substring(0,3));
            if(mawb.length() >= 7) dictionary.put(ReportConstants.MAWB47, mawb.substring(3,7));
            if(mawb.length() >= 11) dictionary.put(ReportConstants.MAWB811, mawb.substring(7,mawb.length()));
            dictionary.put(ReportConstants.MAWB_NUMBER, mawb);
        } else {
            dictionary.put(ReportConstants.MAWB_NUMBER, null);
        }
        if(awbLabelModel.shipment.getCarrierDetails() != null)
            dictionary.put(ReportConstants.DESTINATION, awbLabelModel.shipment.getCarrierDetails().getDestinationPort());
        if(awbLabelModel.shipment.getInnerPacks() != null){
            var inners = awbLabelModel.shipment.getInnerPacks().toString();
            int size = inners.length();
            if(size < 4){
                for (int i=0; i<4-size; i++){
                    inners = "0" + inners;
                }
            }
            dictionary.put(ReportConstants.INNERS, inners);
        }
        if(awbLabelModel.shipment.getNoOfPacks() != null){
            var packs = awbLabelModel.shipment.getNoOfPacks().toString();
            int size = packs.length();
            if(size < 4){
                for (int i=0; i<4-size; i++){
                    packs = "0" + packs;
                }
            }
            dictionary.put(ReportConstants.PACKS, packs);
        }
        if(awbLabelModel.tenant != null) {
            ReportHelper.addTenantDetails(dictionary, awbLabelModel.tenant);
            awbLabelModel.setTenantAddress(ReportHelper.getListOfStrings(awbLabelModel.tenant.tenantName, awbLabelModel.tenant.address1,
                    awbLabelModel.tenant.address2, awbLabelModel.tenant.city, awbLabelModel.tenant.state,
                    awbLabelModel.tenant.zipPostCode, awbLabelModel.tenant.country, awbLabelModel.tenant.email,
                    awbLabelModel.tenant.websiteUrl, awbLabelModel.tenant.phone));
            if (awbLabelModel.getTenantAddress() != null)
                dictionary.put(ReportConstants.TENANT, awbLabelModel.getTenantAddress());
        }
        dictionary.put(ReportConstants.HAWB_NUMBER, awbLabelModel.shipment.getHouseBill());

        List<Object>  criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "=",
                awbLabelModel.shipment.getCarrierDetails().getDestination()
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse != null && !unlocationsResponse.isEmpty())
            dictionary.put(ReportConstants.HDEST, unlocationsResponse.get(0).getName());
        return dictionary;
    }
}

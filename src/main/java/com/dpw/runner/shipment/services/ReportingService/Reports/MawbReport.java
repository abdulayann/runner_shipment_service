package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.service.impl.ConsolidationService;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MawbReport extends IReport {

    @Autowired
    private HawbReport hawbReport;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ShipmentService shipmentService;
    @Autowired
    private ConsolidationService consolidationService;

    public boolean isDMawb;


    @Override
    public Map<String, Object> getData(Long id) {
        validatePrinting(id);
        HawbModel mawbModel = (HawbModel) getDocumentModel(id);
        return populateDictionary(mawbModel);
    }

    public void validatePrinting(Long id) {
        Boolean validPrinting = Boolean.TRUE;
        List<String> failureReasons = new ArrayList<>();
        if (!isDMawb) {
            ConsolidationDetails consolidation = getConsolidationsById(id);
            if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(consolidation.getTransportMode())
                    && Constants.DIRECTION_EXP.equalsIgnoreCase(consolidation.getShipmentType())
                    && (Constants.CARGO_TYPE_FCL.equalsIgnoreCase(consolidation.getContainerCategory())
                        || Constants.SHIPMENT_TYPE_LCL.equalsIgnoreCase(consolidation.getShipmentType()))
                    && ObjectUtils.isNotEmpty(consolidation.getConsolidationType())
                    && !Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(consolidation.getConsolidationType())) {

                            validPrinting = consolidationService.validateCarrierDetails(consolidation, validPrinting, failureReasons);
                            validPrinting = consolidationService.validateContainerDetails(consolidation, validPrinting, failureReasons);

                        }


        } else {
            ShipmentDetails shipment = getShipmentDetails(id);
            if (Constants.TRANSPORT_MODE_SEA.equalsIgnoreCase(shipment.getTransportMode())
                    && Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection())
                    && ((Constants.CARGO_TYPE_FCL.equalsIgnoreCase(shipment.getShipmentType())
                        || Constants.SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipment.getShipmentType())))
                    && ObjectUtils.isNotEmpty(shipment.getJobType())
                    && !Constants.SHIPMENT_TYPE_DRT.equalsIgnoreCase(shipment.getJobType())) {

                            validPrinting = shipmentService.validateCarrierDetails(shipment, validPrinting, failureReasons);
                            validPrinting = shipmentService.validateContainerDetails(shipment, validPrinting, failureReasons);

            }
        }

        if (validPrinting.equals(Boolean.FALSE)) {
            throw new ReportException(String.join(" -", failureReasons));
        }
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HawbModel hawbModel = new HawbModel();
        if (!isDMawb) {
            hawbModel.usersDto = UserContext.getUser();
            hawbModel.setConsolidationDetails(getConsolidation(id));
            validateAirDGCheckConsolidations(hawbModel.getConsolidationDetails());
            String entityType = "MAWB";
            hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
            hawbModel.awb = hawbModel.getMawb();
            hawbModel.setEntityType(entityType);
        } else {
            hawbModel.usersDto = UserContext.getUser();
            hawbModel.shipmentDetails = getShipment(id);
            validateAirDGCheckShipments(hawbModel.shipmentDetails);
            String entityType = "MAWB";
            if(hawbModel.shipmentDetails != null && hawbModel.shipmentDetails.getConsolidationList() != null && !hawbModel.shipmentDetails.getConsolidationList().isEmpty())
            {
                hawbModel.setConsolidationDetails(hawbModel.shipmentDetails.getConsolidationList().get(0));
                hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
                hawbModel.awb = hawbModel.getMawb();
            }
            if(hawbModel.getMawb() == null){
                hawbModel.awb = getHawb(id);
                entityType = "DMAWB";
            }
            hawbModel.setEntityType(entityType);
        }

        return hawbModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        HawbModel model = (HawbModel) documentModel;
        var dictionary =  hawbReport.populateDictionary(documentModel);
        if(model.getConsolidationDetails() != null) {
            populateRaKcDataConsolidation(dictionary, model.getConsolidationDetails());
        }
        return dictionary;
    }
}

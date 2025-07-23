package com.dpw.runner.shipment.services.migration.utils;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

@Slf4j
@Component
public class MigrationValidationUtil {

    public void validateLCLAndDGConsolShipment(List<ConsolidationDetails> consolidationDetailsList, List<ShipmentDetails> shipmentDetailsList) {
        Set<String> errors = checkDGLCLConsolidations(consolidationDetailsList);

        checkDGLCLShipments(shipmentDetailsList, errors);

        if (!errors.isEmpty()) {
            String errorMsg = String.join("\n", errors);
            throw new ValidationException(errorMsg);
        }
    }

    private Set<String> checkDGLCLConsolidations(List<ConsolidationDetails> consolidationDetailsList) {
        Set<String> errors = new LinkedHashSet<>();
        long lclConsol = 0;
        long dgConsol = 0;
        if (!CommonUtils.listIsNullOrEmpty(consolidationDetailsList)) {
            for (ConsolidationDetails consolidationDetails: consolidationDetailsList) {
                if (Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())) {
                    lclConsol = lclConsol + 1;
                }
                if (Boolean.TRUE.equals(consolidationDetails.getHazardous())) {
                    dgConsol = dgConsol + 1;
                }
            }
        }

        if (lclConsol > 0) {
            errors.add(String.format("There are %s LCL Consolidations, please correct the data", lclConsol));
        }

        if (dgConsol > 0) {
            errors.add(String.format("There are %s DG Consolidations, please correct the data", dgConsol));
        }
        return errors;
    }

    private void checkDGLCLShipments(List<ShipmentDetails> shipmentDetailsList, Set<String> errors) {
        long lclShipment = 0;
        long dgShipment = 0;

        if (!CommonUtils.listIsNullOrEmpty(shipmentDetailsList)) {
            for (ShipmentDetails shipmentDetails: shipmentDetailsList) {
                if (Constants.SHIPMENT_TYPE_LCL.equals(shipmentDetails.getShipmentType())) {
                    lclShipment = lclShipment + 1;
                }
                if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                    dgShipment = dgShipment + 1;
                }
            }
        }

        if (lclShipment > 0) {
            errors.add(String.format("There are %s LCL Shipments, please correct the data", lclShipment));
        }

        if (dgShipment > 0) {
            errors.add(String.format("There are %s DG Shipments, please correct the data", dgShipment));
        }
    }

}

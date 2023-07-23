package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IContainerService extends ICommonService {
    ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);
    ResponseEntity<?> calculateAchievedQuantity_onPackAssign(CommonRequestModel commonRequestModel);
    ResponseEntity<?> calculateAchievedQuantity_onPackDetach(CommonRequestModel commonRequestModel);
    ResponseEntity<?> getContainersForSelection(CommonRequestModel commonRequestModel);
}

package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.dto.v3.request.VgmCancelRequest;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.kafka.dto.inttra.VgmEventDto;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

import jakarta.validation.Valid;
import java.util.List;

public interface IVerifiedGrossMassService {

    /**
     * Create a new Verified Gross mass.
     *
     * @param request VerifiedGrossMassRequest
     * @return VerifiedGrossMassResponse
     */
    VerifiedGrossMassResponse create(VerifiedGrossMassRequest request);

    /**
     * Retrieve a Verified Gross mass by its ID.
     *
     * @param id booking id
     * @return VerifiedGrossMassResponse
     */
    VerifiedGrossMassResponse retrieveById(Long id);

    ResponseEntity<IRunnerResponse> list(CommonRequestModel listCommonRequest, boolean getMasterData);

    Page<VerifiedGrossMass> getVerifiedGrossMasses(ListCommonRequest listCommonRequest);

    /**
     * Update an existing Verified Gross mass.
     *
     *
     * @param request VerifiedGrossMassRequest
     * @return VerifiedGrossMassResponse
     */
    VerifiedGrossMassResponse update(VerifiedGrossMassRequest request);

    /**
     * Delete a Verified Gross mass by its ID.
     *
     * @param id booking id
     */
    void delete(Long id);

    ResponseEntity<IRunnerResponse> getAllMasterData(Long vgmId);

    VerifiedGrossMassResponse getDefaultVerifiedGrossMassValues(EntityType type, Long entityId);

    List<CommonContainerResponse> bulkUpdateContainers(VerifiedGrossMassBulkUpdateRequest request);

    /**
     * Submit/Amend Verified Gross mass.
     *
     * @param submitAmendInttraRequest request
     *
     */
    void submitOrAmendVerifiedGrossMass(SubmitAmendInttraRequest submitAmendInttraRequest) throws RunnerException;

    void updateVgmStatus(VgmEventDto vgm);

    /**
     * Sync Verified Gross mass Containers from Console.
     *
     * @param commonContainerIds list of container ids
     * @return list of CommonContainerResponse
     */
    List<CommonContainerResponse> syncContainersByIds(List<Long> commonContainerIds);

    void cancelVerifiedGrossMass(@Valid VgmCancelRequest id);
}


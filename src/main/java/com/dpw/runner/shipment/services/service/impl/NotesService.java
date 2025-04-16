package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NotesConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.NotesResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.INotesService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class NotesService implements INotesService {
    @Autowired
    private INotesDao notesDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    @Autowired
    private ShipmentService shipmentService;

    @Autowired
    private ConsolidationService consolidationService;

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            NotesRequest request = (NotesRequest) commonRequestModel.getData();
            if(request.getEntityId() == null) {
                if(StringUtility.isEmpty(request.getEntityGuid()) || StringUtility.isEmpty(request.getEntityType())) {
                    log.debug(NotesConstants.NOTES_REQUEST_ID_NULL, LoggerHelper.getRequestIdFromMDC());
                    throw new RunnerException(NotesConstants.NOTES_ENTITY_ID_NOT_PRESENT);
                }
                CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(request.getEntityGuid()).build();
                if(request.getEntityType().equalsIgnoreCase(Constants.SHIPMENT)) {
                    ResponseEntity<IRunnerResponse> response = shipmentService.getIdFromGuid(CommonRequestModel.buildRequest(commonGetRequest));
                    ShipmentDetailsResponse shipmentDetailsResponse = (ShipmentDetailsResponse) ((RunnerResponse<?>) Objects.requireNonNull(response.getBody())).getData();
                    validateShipmentDetailsResponse(shipmentDetailsResponse);
                    request.setEntityId(shipmentDetailsResponse.getId());
                } else if(request.getEntityType().equalsIgnoreCase(Constants.CONSOLIDATION)) {
                    ResponseEntity<IRunnerResponse> response = consolidationService.getIdFromGuid(CommonRequestModel.buildRequest(commonGetRequest));
                    ConsolidationDetailsResponse consolidationDetailsResponse = (ConsolidationDetailsResponse) ((RunnerResponse<?>) Objects.requireNonNull(response.getBody())).getData();
                    validateConsoleDetailsResponse(consolidationDetailsResponse);
                    request.setEntityId(consolidationDetailsResponse.getId());
                } else {
                    log.debug(NotesConstants.NOTES_REQUEST_ID_NULL, LoggerHelper.getRequestIdFromMDC());
                    throw new RunnerException(NotesConstants.NOTES_ENTITY_ID_NOT_PRESENT);
                }
            }
            Notes notes = convertRequestToNotesEntity(request);
            notes = notesDao.save(notes);
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(notes)
                            .prevData(null)
                            .parent(request.getEntityType())
                            .parentId(request.getEntityId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Notes Details created successfully for Id {} with Request Id {}", notes.getId(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void validateConsoleDetailsResponse(ConsolidationDetailsResponse consolidationDetailsResponse) throws RunnerException {
        if(consolidationDetailsResponse == null) {
            log.debug(NotesConstants.NOTES_REQUEST_ID_NULL, LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(NotesConstants.NOTES_ENTITY_ID_NOT_PRESENT);
        }
    }

    private void validateShipmentDetailsResponse(ShipmentDetailsResponse shipmentDetailsResponse) throws RunnerException {
        if(shipmentDetailsResponse == null) {
            log.debug(NotesConstants.NOTES_REQUEST_ID_NULL, LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(NotesConstants.NOTES_ENTITY_ID_NOT_PRESENT);
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        NotesRequest request = (NotesRequest) commonRequestModel.getData();

        if(request.getId() == null) {
            log.debug("Request Id is null for Notes update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = request.getId();
        Optional<Notes> oldEntity = notesDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(NotesConstants.NOTES_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Notes notes = convertRequestToNotesEntity(request);
        notes.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            notes = notesDao.save(notes);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(notes)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Notes.class))
                            .parent(request.getEntityType())
                            .parentId(request.getEntityId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the Notes details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<Notes>, Pageable> tuple = fetchData(request, Notes.class);
            Page<Notes> notesPage = notesDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Notes list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(notesPage.getContent()),
                    notesPage.getTotalPages(),
                    notesPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<Notes>, Pageable> tuple = fetchData(request, Notes.class);
            Page<Notes> notesPage = notesDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Notes async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(notesPage.getContent()),
                    notesPage.getTotalPages(),
                    notesPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null) {
                log.debug("Request Id is null for Notes delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Long id = request.getId();

            Optional<Notes> note = notesDao.findById(id);
            if (note.isEmpty()) {
                log.debug(NotesConstants.NOTES_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String parent = note.get().getEntityType();
            Long parentId = note.get().getEntityId();
            String oldEntityJsonString = jsonHelper.convertToJson(note.get());
            notesDao.delete(note.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Notes.class))
                            .parent(parent)
                            .parentId(parentId)
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted notes for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null) {
                log.error("Request Id is null for Notes retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Long id = request.getId();
            Optional<Notes> notes = notesDao.findById(id);
            if (notes.isEmpty()) {
                log.debug(NotesConstants.NOTES_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Notes details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            NotesResponse response = convertEntityToDto(notes.get());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(response,request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private NotesResponse convertEntityToDto(Notes notes) {
        return jsonHelper.convertValue(notes, NotesResponse.class);
    }

    private Notes convertRequestToNotesEntity(NotesRequest request) {
        return jsonHelper.convertValue(request, Notes.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<Notes> lst) {
        return lst.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }
}

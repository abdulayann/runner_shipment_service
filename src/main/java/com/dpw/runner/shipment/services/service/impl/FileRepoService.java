package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IFileRepoDao;
import com.dpw.runner.shipment.services.dto.request.EntityIdAndTypeRequest;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.request.EntityIdAndTypeRequest;
import com.dpw.runner.shipment.services.dto.response.FileRepoResponse;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IFileRepoService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;

@Service
@Slf4j
public class FileRepoService implements IFileRepoService {
    @Autowired
    private IFileRepoDao fileRepoDao;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        FileRepoRequest request = null;
        request = (FileRepoRequest) commonRequestModel.getData();
        FileRepo fileRepo = mapToEntityFromRequest(request);
        fileRepo = fileRepoDao.save(fileRepo);
        if(request == null) {
            log.debug("Request is empty for File create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        try {
            fileRepo = fileRepoDao.save(fileRepo);
            log.info("File created for successfully for Id {} with Request Id {}", fileRepo.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertToResponse(fileRepo));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        FileRepoRequest request = (FileRepoRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for File update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for File update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<FileRepo> oldEntity = fileRepoDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("File Repo is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        FileRepo fileRepo = mapToEntityFromRequest(request);
        fileRepo.setId(oldEntity.get().getId());
        try {
            fileRepo = fileRepoDao.save(fileRepo);
            log.info("Updated the File details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertToResponse(fileRepo));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for list File with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<FileRepo>, Pageable> tuple = fetchData(request, FileRepo.class);
            Page<FileRepo> FileRepoPage  = fileRepoDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("File list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertListResponse(FileRepoPage.getContent()),
                    FileRepoPage.getTotalPages(),
                    FileRepoPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for async list File with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            log.info("Retrieving File details");
            Pair<Specification<FileRepo>, Pageable> tuple = fetchData(request, FileRepo.class);
            Page<FileRepo> FileRepoPage  = fileRepoDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("File async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertListResponse(FileRepoPage.getContent()),
                    FileRepoPage.getTotalPages(),
                    FileRepoPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid is null for File delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for File delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getGuid() == null) {
                log.error("GUID is null for File delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<FileRepo> fileRepo;
            if(request.getId() != null) {
                long id = request.getId();
                fileRepo = fileRepoDao.findById(id);
                if(!fileRepo.isPresent()) {
                    log.debug("File Repo is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                fileRepo = fileRepoDao.findByGuid(guid);
                if(!fileRepo.isPresent()) {
                    log.debug("File Repo is null for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            fileRepoDao.delete(fileRepo.get());
            log.info("Deleted file for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for File retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid is null for File retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for File retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getGuid() == null) {
                log.error("GUID is null for File retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<FileRepo> fileRepo;
            FileRepoResponse response;
            if(request.getId() != null) {
                long id = request.getId();
                fileRepo = fileRepoDao.findById(id);
                if(!fileRepo.isPresent()) {
                    log.debug("File Repo is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("File details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                fileRepo = fileRepoDao.findByGuid(guid);
                if(!fileRepo.isPresent()) {
                    log.debug("File Repo is null for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                log.info("File details fetched successfully for GUId {} with Request Id {}", guid, LoggerHelper.getRequestIdFromMDC());
            }
            response = convertToResponse(fileRepo.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveByEntityIdAndEntityType(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            EntityIdAndTypeRequest request = (EntityIdAndTypeRequest) commonRequestModel.getData();
            List<FileRepo> fileRepoList = fileRepoDao.findByEntityIdAndEntityType(request.getEntityId(), request.getEntityType());
            if (fileRepoList.size() == 0) {
                ResponseHelper.buildListSuccessResponse(null);
            }
            return ResponseHelper.buildListSuccessResponse(convertListResponse(fileRepoList));

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertListResponse(List<FileRepo> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(fileRepo -> {
            responseList.add(convertToResponse(fileRepo));
        });
        return responseList;
    }

    private FileRepo mapToEntityFromRequest(FileRepoRequest request) {
        return modelMapper.map(request, FileRepo.class);
    }

    private FileRepoResponse convertToResponse(FileRepo fileRepo) {
        return modelMapper.map(fileRepo, FileRepoResponse.class);
    }

}

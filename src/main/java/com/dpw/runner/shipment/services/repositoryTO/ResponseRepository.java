package com.dpw.runner.shipment.services.repositoryTO;


import com.dpw.runner.shipment.services.entityTO.ResponseEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface ResponseRepository extends JpaRepository<ResponseEntity, UUID> {
}

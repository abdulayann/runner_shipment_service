package com.dpw.runner.shipment.services.repository.interfaces;


import com.dpw.runner.shipment.services.entity.ResponseEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface ResponseRepository extends JpaRepository<ResponseEntity, UUID> {
}

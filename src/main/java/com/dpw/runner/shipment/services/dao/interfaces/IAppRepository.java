package com.dpw.runner.shipment.services.dao.interfaces;


import com.dpw.runner.shipment.services.entity.AppConfig;
import java.io.Serializable;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IAppRepository extends JpaRepository<AppConfig, Serializable> {

  Optional<AppConfig> findByKey(String key);
}
